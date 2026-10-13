# cnn_worker.R
#
# Runs the Static Models CNN (Keras/TensorFlow) work in a separate background
# R process instead of the main app process.
#
# Why: TensorFlow's predict() was crashing the main R process on Railway with
# a segfault. A crash like that can't be caught by tryCatch(); it kills the
# whole process, ending every user's session. In a separate process, a crash
# only kills that process: the app shows an error and starts a new one.
# It also keeps TensorFlow's memory out of the main app process.
#
# How:
# - One background R process (a mirai daemon on its own "cnn" profile, separate
#   from the training daemons in background_tasks.R) is started the first time
#   a CNN model is selected. It sources bucket_helper.R, so it downloads and
#   caches the Keras models itself (load_bucket_keras_model_cached).
# - Keras models are Python objects and can't be passed between R processes,
#   so the main app only keeps a reference to the model (its bucket key, see
#   cnn_model_ref()); the background process loads and runs the model.
# - The main app waits for each result (as it did before, when the work ran in
#   the main process). If the background process crashes or doesn't answer
#   within CNN_WORKER_TIMEOUT_SEC (default 120), the call returns an error and
#   the process is replaced for the next request.
# - If mirai is unavailable, the work runs in the main process as before.

CNN_PROFILE <- "cnn"
cnn_worker_running <- FALSE

cnn_worker_timeout_ms <- function() {
  s <- suppressWarnings(as.numeric(Sys.getenv("CNN_WORKER_TIMEOUT_SEC", "120")))
  if (is.na(s) || s <= 0) s <- 120
  s * 1000
}

start_cnn_worker <- function() {
  setup_file <- normalizePath(file.path("modules", "bucket_helper.R"), mustWork = TRUE)
  mirai::daemons(1, dispatcher = FALSE, cleanup = FALSE, output = TRUE, .compute = CNN_PROFILE)
  # Wait for the setup to finish so a failure shows up here
  setup <- mirai::everywhere(source(setup_file), setup_file = setup_file, .compute = CNN_PROFILE)
  if (length(setup)) mirai::call_mirai(setup)
  cnn_worker_running <<- TRUE
  message("CNN worker: background process started")
  invisible(TRUE)
}

stop_cnn_worker <- function() {
  try(mirai::daemons(0, .compute = CNN_PROFILE), silent = TRUE)
  cnn_worker_running <<- FALSE
}

# Run fn(...) in the CNN background process and wait for the result.
# Returns list(ok = TRUE, value) or list(ok = FALSE, error = "message").
run_cnn_job <- function(fn, ...) {
  if (!requireNamespace("mirai", quietly = TRUE)) {
    # Fallback: run in the main process (the old behaviour)
    res <- tryCatch(list(ok = TRUE, value = fn(...)),
                    error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    return(res)
  }

  if (!cnn_worker_running) {
    started <- tryCatch(start_cnn_worker(), error = function(e) {
      message("CNN worker: could not start | ", conditionMessage(e))
      stop_cnn_worker()
      FALSE
    })
    if (!isTRUE(started)) {
      return(list(ok = FALSE, error = "Could not start the CNN background process"))
    }
  }

  m <- mirai::mirai(
    tryCatch(list(ok = TRUE, value = do.call(fn, args)),
             error = function(e) list(ok = FALSE, error = conditionMessage(e))),
    .args = list(fn = fn, args = list(...)),
    .compute = CNN_PROFILE, .timeout = cnn_worker_timeout_ms()
  )
  res <- mirai::call_mirai(m)$data

  if (mirai::is_error_value(res)) {
    # The background process crashed (connection reset) or timed out.
    # Replace it so the next request starts a fresh one.
    message("CNN worker: background process failed (", format(res), "), restarting it on next use")
    stop_cnn_worker()
    return(list(ok = FALSE, error = paste(
      "The CNN model crashed or timed out while running.",
      "The background process has been restarted; please try again."
    )))
  }
  if (inherits(res, "miraiError")) {
    return(list(ok = FALSE, error = conditionMessage(res)))
  }
  res
}

# What the app stores in shared$usedModel for a CNN: the model's bucket key.
cnn_model_ref <- function(model_key) {
  structure(list(key = model_key), class = "cnn_model_ref")
}

# Download (first time) and load the model in the background process, so a
# missing or unreadable model is reported when it's selected, like before.
cnn_load_model <- function(model_key) {
  run_cnn_job(function(key) {
    load_bucket_keras_model_cached(key)
    TRUE
  }, key = model_key)
}

# Predict with the model at model_key. x is a numeric matrix
# (samples x wavenumbers); returns list(ok, value = numeric predictions).
cnn_predict <- function(model_key, x) {
  run_cnn_job(function(key, x) {
    model <- load_bucket_keras_model_cached(key)
    # reshape to (batch, n_features, 1)
    x <- keras::array_reshape(x, c(nrow(x), ncol(x), 1))
    # Call Python's predict() directly; see data_input_main.R for why
    as.numeric(model$predict(x, verbose = 0L))
  }, key = model_key, x = x)
}
