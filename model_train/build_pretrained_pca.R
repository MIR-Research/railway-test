# build_pretrained_pca.R
#
# Builds pretrained_pca/pca_<property>.rds for the Customized Model (KNN)
# page. Not run by the app; run by hand, then upload the output file to the
# bucket's pretrained_pca/ folder.
#
# PCA settings follow Cubist_Global.R (2.Model_training_Codes/Global):
#   SG smoothing (m = 0, w = 13, p = 2) -> resample to 10 cm-1 (4000-600)
#   -> SNV -> prcomp(center = TRUE, scale. = TRUE)
# with one deliberate difference: the PCA is fitted on ALL rows of the file,
# in file order, not on the 70% calibration split. The app finds neighbours
# by row number in pca$x and uses those row numbers to pick samples from
# the calibration data it loads from spectral_data/<property>.txt, so the
# PCA rows must be exactly that file's rows (the existing pretrained files
# are built the same way, e.g. pca_TC.rds has all 73,036 TC rows).
#
# The preprocessing reuses the app's own code (modules/calibration_data.R),
# so the spectra match what the app loads.
#
# Usage (from the railway_test folder):
#   Rscript model_train/build_pretrained_pca.R <property> <input .txt> [output folder]
# e.g.
#   Rscript model_train/build_pretrained_pca.R hpom "Z:/.../1.Full_DFs/hpom.txt" pretrained_pca_out

suppressPackageStartupMessages({
  library(prospectr)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("usage: build_pretrained_pca.R <property> <input .txt> [output folder]")
prop    <- args[1]
src     <- args[2]
out_dir <- if (length(args) >= 3) args[3] else "pretrained_pca_out"

source("modules/calibration_data.R")   # N_META_COLS, RESAMPLE_WAV, build_calibration()

# Same reading and clean-up as read_spectral_file() in calibration_data.R,
# but from a local file instead of the bucket
raw <- data.table::fread(src, sep = ",", data.table = FALSE, showProgress = FALSE)
raw[] <- lapply(raw, function(x) {
  if (is.character(x)) {
    x <- gsub('""', '"', x, fixed = TRUE)
    x[x == ""] <- NA_character_
  }
  if (is.integer(x)) x <- as.numeric(x)
  x
})
stopifnot(ncol(raw) == N_META_COLS + length(RAW_WAV), "calc_value" %in% names(raw))

bundle <- build_calibration(raw)   # list(meta, snv): SG -> resample -> SNV
snv <- bundle$snv

pca <- prcomp(snv, center = TRUE, scale. = TRUE)

obj <- list(
  soil_property   = prop,
  pca             = pca,
  calib_scores_2d = as.data.frame(pca$x[, 1:2, drop = FALSE]),
  spec_cols       = colnames(snv),
  n_calib         = nrow(snv),
  calc_value      = bundle$meta$calc_value
)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_file <- file.path(out_dir, paste0("pca_", prop, ".rds"))
saveRDS(obj, out_file)

cum_var <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
message(sprintf("Saved %s: %d rows, %d wavenumbers, %d components reach 99%% variance",
                out_file, nrow(snv), ncol(snv), which(cum_var >= 0.99)[1]))
