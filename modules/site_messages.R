# site_messages.R
#
# Messages to users (e.g. scheduled maintenance), set without a redeploy.
#
# The message is a small JSON file in the bucket (SITE_MESSAGE_KEY, default
# site_messages/message.json), written by set_site_message() in
# tools/site_message.R and removed by clear_site_message(). Each service
# reads it once a minute (one read per service, not per user):
# - no file, an empty message, or outside its start/end times -> nothing is
#   shown at all
# - otherwise current users get a pop-up within a minute, and new visitors
#   get it when they open the site. Each person sees a given message once;
#   setting a new message shows it again.
#
# start_site_message_poller() is called from app.R at startup, and
# siteMessageServer(session) from server().

site_message_key  <- Sys.getenv("SITE_MESSAGE_KEY", "site_messages/message.json")
site_message_poll <- suppressWarnings(as.numeric(Sys.getenv("SITE_MESSAGE_POLL_SEC", "60")))
if (is.na(site_message_poll) || site_message_poll < 10) site_message_poll <- 60

site_msg_state <- new.env(parent = emptyenv())
site_msg_state$message    <- NULL   # list(id, title, message, type, start, end), or NULL
site_msg_state$last_error <- ""

# The message as stored in the bucket, or NULL if there is none.
# (A missing file is the normal "no message" case, not an error.)
# Problems other than "no file" are raised as errors, so refresh_site_message()
# logs them and the status link (?status) shows them.
read_site_message <- function() {
  raw <- suppressWarnings(aws.s3::get_object(
    object = site_message_key, bucket = bucket_name(),
    key = s3_opts()$key, secret = s3_opts()$secret, region = s3_opts()$region,
    base_url = s3_opts()$base_url, use_https = s3_opts()$use_https,
    url_style = s3_opts()$url_style, check_region = s3_opts()$check_region
  ))
  # aws.s3 returns an error object (not raw bytes) when the request failed
  if (!is.raw(raw)) {
    code <- tryCatch(as.character(raw$Code), error = function(e) "")
    if (identical(code, "NoSuchKey")) return(NULL)   # no message set: normal
    stop("bucket read failed: ", if (length(code) && nzchar(code)) code else "unknown error",
         tryCatch(paste0(" (", as.character(raw$Message), ")"), error = function(e) ""))
  }
  if (length(raw) == 0) return(NULL)
  msg <- tryCatch(jsonlite::fromJSON(rawToChar(raw), simplifyVector = TRUE),
                  error = function(e) stop("message file isn't valid JSON: ", conditionMessage(e)))
  if (!is.list(msg)) stop("message file has an unexpected format")
  msg
}

# The message only counts if it has text and the current time is inside its
# (optional) start/end window
site_message_active <- function(msg, now = Sys.time()) {
  if (is.null(msg) || !is.character(msg$message) || !nzchar(trimws(msg$message[1]))) return(FALSE)
  t <- function(x) if (is.null(x) || !nzchar(x)) NA else as.POSIXct(x, tz = "UTC")
  start <- t(msg$start_utc); end <- t(msg$end_utc)
  (is.na(start) || now >= start) && (is.na(end) || now <= end)
}

refresh_site_message <- function() {
  site_msg_state$last_check <- Sys.time()
  ok <- TRUE
  msg <- tryCatch(read_site_message(), error = function(e) {
    ok <<- FALSE
    if (!identical(site_msg_state$last_error, conditionMessage(e))) {
      message("Site message check failed | ", conditionMessage(e))
    }
    site_msg_state$last_error <- conditionMessage(e)
    NULL
  })
  if (ok) site_msg_state$last_error <- ""
  site_msg_state$in_bucket <- msg   # what the file says, even if outside its time window
  new <- if (site_message_active(msg)) msg else NULL
  if (!identical(new$id, site_msg_state$message$id)) {
    message(if (is.null(new)) "Site message: none" else paste("Site message: showing", new$id))
  }
  site_msg_state$message <- new
}

# For the status link (?status): what this service sees
site_message_status <- function() {
  inb <- site_msg_state$in_bucket
  list(
    showing     = if (is.null(site_msg_state$message)) "nothing" else site_msg_state$message$id,
    in_bucket   = if (is.null(inb)) "no message file"
                  else sprintf("message %s, start %s, end %s", inb$id,
                               if (nzchar(inb$start_utc %||% "")) paste(inb$start_utc, "UTC") else "now",
                               if (nzchar(inb$end_utc %||% "")) paste(inb$end_utc, "UTC") else "until cleared"),
    last_check  = if (is.null(site_msg_state$last_check)) "not yet"
                  else format(site_msg_state$last_check, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
    error       = if (nzchar(site_msg_state$last_error)) site_msg_state$last_error else "none",
    bucket      = bucket_name(),
    file        = site_message_key
  )
}

start_site_message_poller <- function() {
  tick <- function() {
    refresh_site_message()
    later::later(tick, site_message_poll)
  }
  tick()
  invisible(NULL)
}

show_site_message <- function(session, msg) {
  type  <- if (identical(msg$type, "warning")) "warning" else "info"
  title <- if (is.character(msg$title) && nzchar(msg$title)) msg$title
           else if (type == "warning") "Notice" else "Message"
  showModal(modalDialog(
    title = tagList(if (type == "warning") icon("triangle-exclamation") else icon("circle-info"), " ", title),
    # Line breaks in the message are kept
    tags$div(style = "white-space: pre-wrap;", msg$message),
    easyClose = TRUE,
    footer = modalButton("OK")
  ), session = session)
}

# Per session: show the active message once (now, if there is one, and
# whenever a new one appears)
siteMessageServer <- function(session) {
  seen <- character(0)
  check <- function() {
    msg <- site_msg_state$message
    if (is.null(msg) || msg$id %in% seen) return()
    seen <<- c(seen, msg$id)
    show_site_message(session, msg)
  }
  # First look 3 s after the page opens (so the page has loaded), then every
  # 15 s. This only reads the copy in memory; the bucket is read once a
  # minute by start_site_message_poller().
  first <- TRUE
  observe({
    invalidateLater(if (first) 3000 else 15000)
    if (first) { first <<- FALSE; return() }
    isolate(check())
  })
  invisible(NULL)
}
