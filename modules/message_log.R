# message_log.R
#
# Keeps a per-user log of the error and warning notifications shown during a
# session, viewable from a small "Messages" tab in the bottom-left corner.
# Popups still appear as before; the log just lets users read them again.
#
# How messages are captured: app.R sources every module into the global
# environment, so the modules' showNotification() calls find the version
# defined below before shiny's. It records errors and warnings, then calls
# shiny::showNotification() so the popup behaves exactly as before.

MESSAGE_LOG_TYPES <- c("error", "warning")
MESSAGE_LOG_MAX   <- 200   # keep the most recent entries per session

showNotification <- function(ui, action = NULL, duration = 5, closeButton = TRUE,
                             id = NULL, type = c("default", "message", "warning", "error"),
                             session = getDefaultReactiveDomain()) {
  type <- match.arg(type)
  if (type %in% MESSAGE_LOG_TYPES) {
    try(log_message(ui, type, session), silent = TRUE)  # never break the popup
  }
  shiny::showNotification(ui, action = action, duration = duration,
                          closeButton = closeButton, id = id, type = type,
                          session = session)
}

# Plain text of a notification (it may be a string or HTML tags).
notification_text <- function(ui) {
  txt <- paste(as.character(ui), collapse = " ")
  trimws(gsub("<[^>]+>", "", txt))
}

log_message <- function(ui, type, session) {
  if (is.null(session)) return(invisible())
  log <- session$userData$message_log   # shared by the session and its modules
  if (is.null(log)) return(invisible())
  entry <- data.frame(time = format(Sys.time(), "%H:%M:%S"), type = type,
                      text = notification_text(ui), stringsAsFactors = FALSE)
  isolate(log(utils::head(rbind(entry, log()), MESSAGE_LOG_MAX)))  # newest first
  invisible()
}

messageLogUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      #message-log-tab {
        position: fixed; left: 16px; bottom: 16px; z-index: 1050;
      }
      #message-log-tab .msg-toggle {
        background: #fff; border: 1px solid #999; border-radius: 4px;
        padding: 4px 10px; font-size: 0.9rem; color: #030303; cursor: pointer;
        box-shadow: 0 1px 4px rgba(0,0,0,0.2);
      }
      #message-log-tab .msg-count {
        display: inline-block; min-width: 1.4em; margin-left: 4px; padding: 0 5px;
        border-radius: 9px; background: #6c757d; color: #fff; text-align: center;
      }
      #message-log-tab .msg-count.has-errors { background: #FF4500; }
      #message-log-tab .msg-panel {
        position: absolute; left: 0; bottom: 2.4rem; width: min(420px, 90vw);
        max-height: 50vh; overflow-y: auto; background: #fff;
        border: 1px solid #999; border-radius: 4px; padding: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.25);
      }
      #message-log-tab .msg-panel ul { list-style: none; padding: 0; margin: 0; }
      #message-log-tab .msg-panel li {
        border-left: 4px solid #FFD700; padding: 4px 8px; margin-bottom: 6px;
        background: #fafafa; font-size: 0.85rem; overflow-wrap: anywhere;
      }
      #message-log-tab .msg-panel li.msg-error { border-left-color: #FF4500; }
      #message-log-tab .msg-meta { color: #555; font-size: 0.75rem; }
    ")),
    tags$div(
      id = "message-log-tab",
      tags$button(
        type = "button", class = "msg-toggle",
        `aria-expanded` = "false", `aria-controls` = ns("panel"),
        onclick = sprintf(
          "var p = document.getElementById('%s'); var open = p.hidden; p.hidden = !open; this.setAttribute('aria-expanded', open);",
          ns("panel")),
        "Messages", uiOutput(ns("count"), inline = TRUE)
      ),
      tags$div(
        id = ns("panel"), class = "msg-panel", hidden = NA,
        role = "log", `aria-live` = "polite", `aria-label` = "Error and warning messages",
        tags$div(
          class = "d-flex justify-content-between align-items-center mb-2",
          tags$strong("Errors & warnings"),
          actionLink(ns("clear"), "Clear")
        ),
        uiOutput(ns("entries"))
      )
    )
  )
}

messageLogServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    log <- reactiveVal(data.frame(time = character(), type = character(),
                                  text = character(), stringsAsFactors = FALSE))
    session$userData$message_log <- log

    observeEvent(input$clear, log(log()[0, ]))

    output$count <- renderUI({
      n <- nrow(log())
      tags$span(class = paste("msg-count", if (any(log()$type == "error")) "has-errors"), n)
    })

    output$entries <- renderUI({
      entries <- log()
      if (nrow(entries) == 0) return(tags$p(class = "text-muted mb-0", "No errors or warnings yet."))
      tags$ul(lapply(seq_len(nrow(entries)), function(i) {
        tags$li(
          class = paste0("msg-", entries$type[i]),
          tags$div(class = "msg-meta", entries$time[i], " • ", toupper(entries$type[i])),
          entries$text[i]   # rendered as text, so message content can't inject HTML
        )
      }))
    })

    outputOptions(output, "count",   suspendWhenHidden = FALSE)
    outputOptions(output, "entries", suspendWhenHidden = FALSE)
  })
}
