# build_kssl_reference.R
#
# Builds data/kssl_reference_full.rds, the KSSL reference line for the
# "Compare with KSSL" tab of the Calibration Transfer plot (Data
# Preprocessing page). The app only needs the mean and SD of every KSSL
# spectrum at each wavenumber, so they are computed once here from the full
# preprocessed KSSL file (~340 MB) and stored in a file of a few KB that
# ships with the app. Not run by the app; rerun by hand if the KSSL file
# changes, then commit the new .rds.
#
# Usage (from the railway_test folder):
#   Rscript model_train/build_kssl_reference.R "<path to KSSL_FUll_preprocessed.csv>"

args <- commandArgs(trailingOnly = TRUE)
src <- if (length(args)) args[1] else
  "Z:/Special/MIR_prediction_platform/Elliott_Malithi_working_folder/4.Transfer_learning/SST/Data/KSSL_FUll_preprocessed.csv"

wavenumbers <- as.character(seq(4000, 600, by = -10))   # same grid as the SST model

# Read only the spectral columns
spec <- data.table::fread(src, select = wavenumbers, showProgress = FALSE)
stopifnot(identical(names(spec), wavenumbers))
spec <- as.matrix(spec)

complete <- rowSums(is.na(spec)) == 0
spec <- spec[complete, , drop = FALSE]

ref <- list(
  label       = "Full KSSL",
  n           = nrow(spec),
  mean        = colMeans(spec),
  sd          = apply(spec, 2, sd),
  wavenumbers = wavenumbers,
  source      = basename(src),
  created     = Sys.Date()
)
names(ref$mean) <- names(ref$sd) <- wavenumbers

dir.create("data", showWarnings = FALSE)
saveRDS(ref, "data/kssl_reference_full.rds")
message(sprintf("Saved data/kssl_reference_full.rds: %d spectra (%d dropped for missing values), %d wavenumbers",
                ref$n, sum(!complete), length(wavenumbers)))
