# parallel_training.R
#
# Lets caret::train() run its cross-validation fits on several CPU cores.
# caret parallelises automatically through foreach once a parallel backend is
# registered; this file creates a pool of worker R processes at app startup
# and registers it. The registration is global, so it applies to every
# train() call in the app (Customized Model and Build Your Own Model).
#
# Design choices:
# - Workers are fresh R processes (PSOCK), not forks of the app. Forking a
#   process that has TensorFlow loaded (after any CNN use) can hang.
# - Worker count comes from parallelly::availableCores(), which respects the
#   container's CPU limit (parallel::detectCores() can report the host's
#   cores on Railway). One core is left for the app itself. Override with
#   TRAINING_WORKERS.
# - Each worker's BLAS is limited to 1 thread so N workers use N cores
#   instead of each also spawning a thread per core.

training_cluster <- NULL

training_worker_count <- function() {
  n <- suppressWarnings(as.integer(Sys.getenv("TRAINING_WORKERS", "")))
  if (is.na(n) || n < 1) n <- parallelly::availableCores(omit = 1)
  max(1L, as.integer(n))
}

start_training_cluster <- function() {
  n <- training_worker_count()
  if (n < 2) {
    foreach::registerDoSEQ()   # only one core available: train sequentially
    message("Parallel training: 1 core available, training sequentially")
    return(invisible(NULL))
  }
  cl <- parallelly::makeClusterPSOCK(
    n,
    rscript_envs = c(OPENBLAS_NUM_THREADS = "1", OMP_NUM_THREADS = "1"),
    verbose = FALSE
  )
  doParallel::registerDoParallel(cl)
  training_cluster <<- cl
  message("Parallel training: ", n, " worker processes started")
  invisible(cl)
}

# Call right before a training job. Takes milliseconds when the workers are
# healthy; if any worker has died, the pool is rebuilt so training still runs.
ensure_training_cluster <- function() {
  if (is.null(training_cluster)) return(invisible(NULL))   # sequential mode
  alive <- tryCatch({
    parallel::clusterEvalQ(training_cluster, TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (!alive) {
    message("Parallel training: a worker stopped responding, restarting the pool")
    try(parallel::stopCluster(training_cluster), silent = TRUE)
    training_cluster <<- NULL
    start_training_cluster()
  }
  invisible(NULL)
}

# Start the pool when the app loads. If that fails for any reason, fall back
# to sequential training rather than stopping the app from starting.
tryCatch(
  start_training_cluster(),
  error = function(e) {
    message("Parallel training unavailable, training sequentially | ", conditionMessage(e))
    foreach::registerDoSEQ()
  }
)
