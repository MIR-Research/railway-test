# load_balancer.R
#
# Spreads users over several copies of the app ("overflow"): each copy is a
# separate Railway service with one replica and its own URL. Replicas can't
# be used because Railway sends each request to a random replica, so a
# user's uploads/downloads could reach a copy that doesn't have their session.
#
# How it works:
# - Each copy counts its own active sessions (+1 when a visitor's session
#   starts, -1 when it ends).
# - A new visitor arriving at a copy that already has MAX_USERS sessions is
#   sent straight on to NEXT_SERVICE_URL, before they've done anything.
# - The last copy (no NEXT_SERVICE_URL) shows a "busy" message instead.
# - Everyone uses the first copy's link; overflow happens automatically.
#
# Settings (Railway variables, so every copy runs identical code):
#   MAX_USERS         sessions allowed on this copy; unset or 0 = no limit
#                     (the load balancer is then off and nothing changes)
#   NEXT_SERVICE_URL  where to send visitors when this copy is full;
#                     leave empty on the last copy
#
# Used from the top of server() in app.R: if load_balancer_admit(session)
# returns FALSE, the visitor was redirected (or told the site is busy) and
# the rest of the server function is skipped for them.

lb_max_users <- suppressWarnings(as.integer(Sys.getenv("MAX_USERS", "0")))
if (is.na(lb_max_users) || lb_max_users < 0) lb_max_users <- 0L
lb_next_url <- trimws(Sys.getenv("NEXT_SERVICE_URL", ""))

# Each redirect adds 1 to ?lb_hops= in the URL. A visitor passed on this many
# times sees the busy message instead, so a misconfigured NEXT_SERVICE_URL
# (e.g. two copies pointing at each other) can't redirect forever.
lb_max_hops <- 10L

lb_state <- new.env(parent = emptyenv())
lb_state$active <- 0L

load_balancer_enabled <- function() lb_max_users > 0

# Number of times this visitor has already been passed on (from ?lb_hops=)
lb_hops <- function(session) {
  query <- parseQueryString(isolate(session$clientData$url_search) %||% "")
  hops <- suppressWarnings(as.integer(query$lb_hops))
  if (length(hops) == 0 || is.na(hops)) 0L else hops
}

lb_show_busy <- function(session) {
  showModal(modalDialog(
    title = "The platform is busy",
    p("All of our servers are currently in use. Please try again in a few minutes."),
    footer = tags$button(type = "button", class = "btn btn-default",
                         onclick = "window.location.reload();", "Try again"),
    easyClose = FALSE
  ), session = session)
}

# TRUE: this visitor stays on this copy (and is counted). FALSE: they were
# sent to the next copy or shown the busy message.
load_balancer_admit <- function(session) {
  if (!load_balancer_enabled()) return(TRUE)

  if (lb_state$active >= lb_max_users) {
    hops <- lb_hops(session)
    if (nzchar(lb_next_url) && hops < lb_max_hops) {
      sep <- if (grepl("?", lb_next_url, fixed = TRUE)) "&" else "?"
      target <- paste0(lb_next_url, sep, "lb_hops=", hops + 1L)
      message(sprintf("Load balancer: full (%d/%d), sending visitor to %s",
                      lb_state$active, lb_max_users, lb_next_url))
      shinyjs::runjs(sprintf("window.location.replace(%s);",
                             encodeString(target, quote = "'")))
    } else {
      message(sprintf("Load balancer: full (%d/%d), no next service, showing busy message",
                      lb_state$active, lb_max_users))
      lb_show_busy(session)
    }
    return(FALSE)
  }

  lb_state$active <- lb_state$active + 1L
  message(sprintf("Load balancer: session started (%d/%d)", lb_state$active, lb_max_users))
  session$onSessionEnded(function() {
    lb_state$active <- max(0L, lb_state$active - 1L)
    message(sprintf("Load balancer: session ended (%d/%d)", lb_state$active, lb_max_users))
  })
  TRUE
}
