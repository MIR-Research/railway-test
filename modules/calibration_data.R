# calibration_data.R
#
# One loader for the KSSL calibration data used across the app.
#
# Each spectral_data/<property>.txt file has 27 metadata columns followed by
# 1765 raw spectral columns (4000 -> 600 cm^-1 at 1.927 cm^-1 steps). The app
# only ever uses the metadata and the *preprocessed* spectra, so we load the
# file once per property and keep a compact bundle:
#
#   meta : data.frame, the 27 metadata columns (calc_value, strata, lat/long...)
#   snv  : numeric matrix, spectra after Savitzky-Golay -> resample to
#          10 cm^-1 -> SNV (341 columns, named 4000..600)
#
# Bundles are cached in memory (shared by all sessions) with a disk copy
# underneath, so a bundle evicted from memory reloads from local disk instead
# of re-downloading and re-preprocessing the raw file.

library(prospectr)

N_META_COLS  <- 27
RAW_WAV      <- seq(4000, by = -1.927, length.out = 1765)
RESAMPLE_WAV <- seq(4000, 600, by = -10)

calib_mem_mb  <- as.numeric(Sys.getenv("CALIB_CACHE_MB", "1024"))
calib_disk_mb <- as.numeric(Sys.getenv("CALIB_DISK_CACHE_MB", "4096"))

# Disk copies are written uncompressed: compressing a ~200 MB bundle takes
# ~12 s, while the uncompressed file is only ~15% larger.
calibration_cache <- cachem::cache_layered(
  cachem::cache_mem(max_size = calib_mem_mb * 1024^2),
  cachem::cache_disk(dir = file.path("cache_data", "calibration"),
                     max_size = calib_disk_mb * 1024^2,
                     write_fn = function(value, file) saveRDS(value, file, compress = FALSE))
)

# Property codes whose data file has a different name. Everything else is
# spectral_data/<code>.txt.
spectral_object_key <- function(property) {
  file <- switch(property,
                 P_Mehlich3 = "P_Mehlich",
                 property)
  bucket_key("spectral_data", paste0(file, ".txt"))
}

# Read a spectral file from the bucket. data.table::fread parses on all cores
# and is ~8x faster than readr on these files; the clean-up afterwards makes
# its output match what read_bucket_delim() (readr) returns, so downstream
# checks like is.na() and == "" behave exactly as before.
read_spectral_file <- function(object_key) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    return(read_bucket_delim(object_key, delim = ","))
  }
  df <- aws.s3::s3read_using(
    FUN = data.table::fread,
    object = object_key,
    bucket = bucket_name(),
    opts = s3_opts(),
    sep = ",",
    data.table = FALSE,
    showProgress = FALSE
  )
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      x <- gsub('""', '"', x, fixed = TRUE)  # readr un-escapes doubled quotes
      x[x == ""] <- NA_character_            # readr reads "" as NA
    }
    if (is.integer(x)) x <- as.numeric(x)    # readr reads numbers as double
    x
  })
  df
}

# prospectr::resample() fits a cubic spline to every spectrum separately,
# which takes ~100 s for the largest properties. Spline interpolation is
# linear in the absorbance values, so resampling onto a fixed grid is the
# same as multiplying by a fixed weight matrix. Its weights decay ~4x per
# point away from each output, so everything past ~25 neighbours is below
# double-precision noise and we store it as a sparse matrix. Results match
# prospectr::resample() to ~1e-13.
resample_weights <- memoise::memoise(function(wav, new_wav) {
  W <- t(apply(diag(length(wav)), 1, function(e) splinefun(x = wav, y = e)(new_wav)))
  W[abs(W) < 1e-16 * max(abs(W))] <- 0
  Matrix::Matrix(W, sparse = TRUE)
})

# SG -> resample -> SNV on the raw spectral columns of `raw`. Each step works
# row by row, so we convert and process 5000 rows at a time straight from
# the data frame. Results are the same as doing it all at once, but a full
# ~1 GB matrix copy of the spectra never exists alongside the data frame.
preprocess_raw_spectra <- function(raw, spec_cols, chunk_rows = 5000) {
  starts <- seq(1, nrow(raw), by = chunk_rows)
  chunks <- lapply(starts, function(s) {
    rows <- s:min(s + chunk_rows - 1, nrow(raw))
    m <- as.matrix(raw[rows, spec_cols, drop = FALSE])
    storage.mode(m) <- "double"
    colnames(m) <- RAW_WAV
    sg  <- savitzkyGolay(m, m = 0, w = 13, p = 2)
    W   <- resample_weights(as.numeric(colnames(sg)), RESAMPLE_WAV)
    res <- as.matrix(sg %*% W)
    standardNormalVariate(res)
  })
  out <- do.call(rbind, chunks)
  dimnames(out) <- list(NULL, RESAMPLE_WAV)  # chunks carry data-frame row names; drop them
  out
}

# Build the bundle from the raw data frame read from the bucket.
build_calibration <- function(raw) {
  meta      <- as.data.frame(raw[, seq_len(N_META_COLS)])
  spec_cols <- (N_META_COLS + 1):(N_META_COLS + length(RAW_WAV))
  list(meta = meta, snv = preprocess_raw_spectra(raw, spec_cols))
}

# Errors are deliberately not caught here: memoise doesn't cache errors, so a
# failed download is retried next time instead of sticking until a restart.
load_calibration_cached <- memoise::memoise(function(property) {
  raw <- read_spectral_file(spectral_object_key(property))
  bundle <- build_calibration(raw)
  rm(raw)
  invisible(gc())
  bundle
}, cache = calibration_cache)

# Returns the bundle for a property, or NULL if it can't be loaded.
load_calibration <- function(property) {
  tryCatch(
    load_calibration_cached(property),
    error = function(e) {
      message("Calibration load failed for: ", property, " | ", conditionMessage(e))
      NULL
    }
  )
}
