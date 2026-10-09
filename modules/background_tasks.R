# background_tasks.R
#
# Runs model training in separate background R processes, so the main app
# process (which serves every user) stays responsive while models train.
# Used with shiny::ExtendedTask in the training modules.
#
# Setup, done once by start_background_trainer() (called from app.R after
# every module is loaded, because it uses training_worker_count() from
# parallel_training.R):
# - Several background processes ("daemons") are started with mirai, one per
#   training slot. Each runs one training job at a time; jobs beyond the
#   number of slots queue until a slot frees up.
# - Each daemon runs parallel_training.R itself, giving it its own pool of
#   TRAINING_WORKERS parallel workers (default 2), so each job uses that many
#   cores for caret's cross-validation fits.
# - Slots default to (available cores - 1) / workers per job, rounded down,
#   leaving one core for the main app. Override with TRAINING_SLOTS.
# - cleanup = FALSE keeps each daemon's global environment between jobs;
#   otherwise mirai would wipe it after each job, taking that daemon's worker
#   pool with it. output = TRUE sends the daemons' messages to the app's log.
#
# If anything here fails, background_training_available stays FALSE and the
# training modules fall back to training in the main process.

background_training_available <- FALSE

training_slot_count <- function() {
  n <- suppressWarnings(as.integer(Sys.getenv("TRAINING_SLOTS", "")))
  if (is.na(n) || n < 1) {
    n <- (parallelly::availableCores() - 1) %/% training_worker_count()
  }
  max(1L, as.integer(n))
}

start_background_trainer <- function() {
  tryCatch({
    if (!requireNamespace("mirai", quietly = TRUE)) stop("package 'mirai' is not installed")
    setup_file <- normalizePath(file.path("modules", "parallel_training.R"), mustWork = TRUE)
    slots <- training_slot_count()

    mirai::daemons(slots, dispatcher = TRUE, cleanup = FALSE, output = TRUE)
    mirai::everywhere(source(setup_file), setup_file = setup_file)

    background_training_available <<- TRUE
    message("Background training: ", slots, " training slot(s), ",
            training_worker_count(), " cores each")
  }, error = function(e) {
    message("Background training unavailable, training in the main process | ", conditionMessage(e))
  })
  invisible(background_training_available)
}
