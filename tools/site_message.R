# site_message.R: set or clear the message shown to website users
#
# Run from R on your own computer (not used by the app itself):
#
#   setwd("C:/Users/ellio/OneDrive/Desktop/railway_test")
#   source("tools/site_message.R")
#
#   set_site_message("The website will be down for maintenance on Oct 30 from 8 to 10 am.")
#   set_site_message("Maintenance tonight from 9 pm.", title = "Scheduled maintenance",
#                    type = "warning", start = "2026-10-29 08:00", end = "2026-10-29 21:00")
#   current_site_message()   # see what's set
#   clear_site_message()     # remove it; nothing is shown any more
#
# Every running service shows the message within about a minute: current
# users get a pop-up, new visitors see it when they open the site, and each
# person sees a given message once. With no message set, nothing pops up.
#
#   start / end : optional, "YYYY-MM-DD HH:MM" in your local time zone (or
#                 the zone given in tz). The message is only shown between
#                 them; leave out to show it right away / until cleared.
#   type        : "info" or "warning"
#
# Needs the same bucket settings the app uses, as environment variables on
# your computer: AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_ENDPOINT_URL
# and AWS_S3_BUCKET_NAME (copy them from a service's Variables in Railway
# into your ~/.Renviron file, then restart R).

source("modules/bucket_helper.R")
site_message_key <- Sys.getenv("SITE_MESSAGE_KEY", "site_messages/message.json")

check_bucket_settings <- function() {
  vars <- c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_ENDPOINT_URL", "AWS_S3_BUCKET_NAME")
  missing <- vars[!nzchar(Sys.getenv(vars))]
  if (length(missing)) stop("Set these environment variables first (see the top of this file): ",
                            paste(missing, collapse = ", "), call. = FALSE)
  # Should match the "bucket" the services report at <service URL>/?status
  message("Using bucket \"", bucket_name(), "\" at ", Sys.getenv("AWS_ENDPOINT_URL"),
          ", file ", site_message_key)
}

to_utc <- function(x, tz) {
  if (is.null(x)) return("")
  t <- as.POSIXct(x, tz = tz, tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d"))
  if (is.na(t)) stop("Couldn't read the time \"", x, "\"; use \"YYYY-MM-DD HH:MM\".", call. = FALSE)
  format(t, "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

set_site_message <- function(message, title = NULL, type = c("info", "warning"),
                             start = NULL, end = NULL, tz = Sys.timezone()) {
  check_bucket_settings()
  type <- match.arg(type)
  if (!is.character(message) || !nzchar(trimws(message))) {
    stop("The message is empty. To remove the message, use clear_site_message().", call. = FALSE)
  }
  msg <- list(
    id        = format(Sys.time(), "%Y%m%d%H%M%OS3", tz = "UTC"),   # new id = shown again
    title     = if (is.null(title)) "" else title,
    message   = message,
    type      = type,
    start_utc = to_utc(start, tz),
    end_utc   = to_utc(end, tz),
    set_at    = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
  if (nzchar(msg$start_utc) && nzchar(msg$end_utc) && msg$end_utc <= msg$start_utc) {
    stop("end is before start.", call. = FALSE)
  }
  tmp <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(msg, auto_unbox = TRUE, pretty = TRUE), tmp, useBytes = TRUE)
  upload_bucket_file(tmp, site_message_key)
  message("Message set. Users will see it within about a minute",
          if (nzchar(msg$start_utc)) paste0(" (from ", start, ")") else "",
          if (nzchar(msg$end_utc)) paste0(" until ", end) else "", ".")
  invisible(msg)
}

clear_site_message <- function() {
  check_bucket_settings()
  ok <- tryCatch({
    aws.s3::delete_object(
      object = site_message_key, bucket = bucket_name(),
      key = s3_opts()$key, secret = s3_opts()$secret, region = s3_opts()$region,
      base_url = s3_opts()$base_url, use_https = s3_opts()$use_https,
      url_style = s3_opts()$url_style, check_region = s3_opts()$check_region
    )
    TRUE
  }, error = function(e) { message("Couldn't delete the message: ", conditionMessage(e)); FALSE })
  if (ok) message("Message cleared. Nothing will pop up for new visitors (within about a minute).")
  invisible(ok)
}

current_site_message <- function() {
  check_bucket_settings()
  raw <- tryCatch(aws.s3::get_object(
    object = site_message_key, bucket = bucket_name(),
    key = s3_opts()$key, secret = s3_opts()$secret, region = s3_opts()$region,
    base_url = s3_opts()$base_url, use_https = s3_opts()$use_https,
    url_style = s3_opts()$url_style, check_region = s3_opts()$check_region
  ), error = function(e) NULL)
  # aws.s3 returns an error object (not raw bytes) when the file is missing
  if (!is.raw(raw) || length(raw) == 0) { message("No message is set."); return(invisible(NULL)) }
  msg <- jsonlite::fromJSON(rawToChar(raw))
  str(msg)
  invisible(msg)
}
