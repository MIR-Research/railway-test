#start-shiny.R

Sys.setenv(RETICULATE_PYTHON = "/opt/venv/bin/python")

port <- as.integer(Sys.getenv("PORT", "3838"))

shiny::runApp(
  appDir = "/app",
  host = "0.0.0.0",
  port = port,
  launch.browser = FALSE
)