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
#   SERVICE_URLS      every service's URL in order, comma-separated; same
#                     value on every service. A full service sends visitors
#                     to the first OTHER service that is online and has room,
#                     skipping offline/full ones (see "Service list" below).
#                     Replaces NEXT_SERVICE_URL, which is ignored when set.
#   NEXT_SERVICE_URL  (older setup) where to send visitors when this copy is
#                     full; leave empty on the last copy
#   IDLE_TIMEOUT_MIN  disconnect a session after this many minutes with no
#                     mouse/keyboard/scroll/touch activity, freeing its
#                     slot (default 30; 0 = never). Sessions with a model
#                     training in the background are never disconnected.
#                     Only active when MAX_USERS is set.
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

lb_idle_timeout_min <- suppressWarnings(as.numeric(Sys.getenv("IDLE_TIMEOUT_MIN", "30")))
if (is.na(lb_idle_timeout_min) || lb_idle_timeout_min < 0) lb_idle_timeout_min <- 30

lb_state <- new.env(parent = emptyenv())
lb_state$active <- 0L
lb_state$probe_cache <- list()

# ---- Service list (preferred over NEXT_SERVICE_URL) ---------------------------
# SERVICE_URLS: every service's URL, in order, separated by commas, e.g.
#   https://svc1.up.railway.app, https://svc2.up.railway.app, https://svc3.up.railway.app
# Give every service the same value (a Railway shared variable works well).
# A full service then checks the others in that order and sends the visitor
# to the first one that is online and has room. Offline or full services are
# skipped, so if service 2 is down, visitors go from 1 straight to 3.
# Each service finds itself in the list from Railway's RAILWAY_PUBLIC_DOMAIN
# (or SELF_URL, if set). When SERVICE_URLS is set, NEXT_SERVICE_URL is ignored.
lb_clean_url <- function(u) sub("/+$", "", trimws(u))
lb_url_host  <- function(u) tolower(sub("^[a-z]+://([^/:?]+).*$", "\\1", lb_clean_url(u)))

lb_service_urls <- local({
  u <- lb_clean_url(strsplit(Sys.getenv("SERVICE_URLS", ""), "[,[:space:]]+")[[1]])
  u[nzchar(u)]
})

lb_self_url <- function() {
  self <- Sys.getenv("SELF_URL", "")
  if (!nzchar(self) && nzchar(Sys.getenv("RAILWAY_PUBLIC_DOMAIN", ""))) {
    self <- paste0("https://", Sys.getenv("RAILWAY_PUBLIC_DOMAIN"))
  }
  lb_clean_url(self)
}

# Each Railway service has its own domain, so comparing domains (and port,
# for local testing) is enough
lb_url_hostport <- function(u) tolower(sub("^[a-z]+://([^/?]+).*$", "\\1", lb_clean_url(u)))
lb_is_self <- function(u) {
  self <- lb_self_url()
  nzchar(self) && identical(lb_url_hostport(u), lb_url_hostport(self))
}

# Ask another service for its own status (?status=self). Results are kept for
# 15 s so a burst of visitors doesn't re-check every service each time.
# Returns list(online, users, max_users) or list(online = FALSE, error).
lb_probe <- function(url, cache_secs = 15) {
  hit <- lb_state$probe_cache[[url]]
  if (!is.null(hit) && difftime(Sys.time(), hit$at, units = "secs") < cache_secs) return(hit$res)
  sep <- if (grepl("?", url, fixed = TRUE)) "&" else "?"
  res <- tryCatch({
    h <- curl::new_handle(timeout = 4, connecttimeout = 3)
    r <- curl::curl_fetch_memory(paste0(url, sep, "status=self"), handle = h)
    if (r$status_code != 200) stop("HTTP ", r$status_code)
    s <- jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE)$services[[1]]
    if (is.null(s$users)) stop("not a status response")
    list(online = TRUE, users = s$users, max_users = s$max_users)
  }, error = function(e) list(online = FALSE, error = conditionMessage(e)))
  lb_state$probe_cache[[url]] <- list(at = Sys.time(), res = res)
  res
}

lb_has_room <- function(p) {
  isTRUE(p$online) && (!is.numeric(p$max_users) || p$users < p$max_users)
}

# First other service (in list order) that is online with room, or NULL
lb_pick_target <- function() {
  for (u in lb_service_urls) {
    if (lb_is_self(u)) next
    p <- lb_probe(u)
    if (lb_has_room(p)) return(u)
    message(sprintf("Load balancer: skipping %s (%s)", u,
                    if (isTRUE(p$online)) sprintf("full, %s/%s", p$users, p$max_users) else paste("offline:", p$error)))
  }
  NULL
}

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
    if (length(lb_service_urls) > 0) {
      # Service list: go to the first other service that is online and has
      # room (checked with its ?status=self), skipping offline/full ones
      target <- if (hops < lb_max_hops) lb_pick_target() else NULL
      if (!is.null(target)) {
        sep <- if (grepl("?", target, fixed = TRUE)) "&" else "?"
        message(sprintf("Load balancer: full (%d/%d), sending visitor to %s",
                        lb_state$active, lb_max_users, target))
        shinyjs::runjs(sprintf("window.location.replace(%s);",
                               encodeString(paste0(target, sep, "lb_hops=", hops + 1L), quote = "'")))
      } else {
        message(sprintf("Load balancer: full (%d/%d), no other service online with room, showing busy message",
                        lb_state$active, lb_max_users))
        lb_show_busy(session)
      }
    } else if (nzchar(lb_next_url) && hops < lb_max_hops) {
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
  lb_start_idle_timer(session)
  TRUE
}

# ---- Idle timeout ------------------------------------------------------------
# Training modules register their background task here, so a session is never
# disconnected while its model trains (the user may be waiting without
# touching anything). Called from knn_models.R and model_training.R.
lb_register_task <- function(session, task) {
  session$userData$lb_tasks <- c(session$userData$lb_tasks, list(task))
  invisible(task)
}

lb_training_running <- function(session) {
  any(vapply(session$userData$lb_tasks %||% list(),
             function(t) identical(isolate(t$status()), "running"), logical(1)))
}

lb_start_idle_timer <- function(session) {
  if (!(lb_idle_timeout_min > 0)) return(invisible(NULL))
  input <- session$input
  last_active <- Sys.time()

  # The browser reports user activity at most every 30 s (an input value
  # change alone isn't enough: reading results or scrolling sends none)
  shinyjs::runjs(paste0(
    "(function(){ var last = 0;",
    "  function ping(){ var now = Date.now();",
    "    if (now - last > 30000) { last = now; Shiny.setInputValue('lb_activity', now); } }",
    "  ['mousemove','mousedown','keydown','scroll','wheel','touchstart'].forEach(function(e){",
    "    document.addEventListener(e, ping, {passive: true, capture: true}); });",
    "})();"
  ))
  observeEvent(input$lb_activity, { last_active <<- Sys.time() }, ignoreInit = TRUE)

  # Check once a minute
  observe({
    invalidateLater(60 * 1000)
    idle_min <- as.numeric(difftime(Sys.time(), last_active, units = "mins"))
    if (idle_min < lb_idle_timeout_min) return()
    if (lb_training_running(session)) return()   # keep sessions that are training
    message(sprintf("Load balancer: disconnecting session idle for %.0f min", idle_min))
    # Say why, then close the session (frees the slot via onSessionEnded)
    shinyjs::runjs(paste0(
      "var d = document.createElement('div');",
      "d.style.cssText = 'position:fixed;inset:0;z-index:100000;display:flex;",
      "align-items:center;justify-content:center;background:rgba(0,0,0,.55);';",
      "d.innerHTML = '<div style=\"background:#fff;padding:24px 28px;border-radius:8px;",
      "max-width:420px;text-align:center;font-size:16px;\">You were disconnected after ",
      lb_idle_timeout_min, " minutes of inactivity.<br><br>",
      "<button class=\"btn btn-default\" onclick=\"window.location.reload()\">Reload</button></div>';",
      "document.body.appendChild(d);"
    ))
    later::later(function() session$close(), 1)
  })
  invisible(NULL)
}


# ---- Status link -------------------------------------------------------------
# <service URL>/?status returns how many users this copy has, followed by
# every copy after it in the chain (each copy asks its NEXT_SERVICE_URL), as
# JSON. So service 1's status link lists all of them. It is answered before
# the app loads, so checking it doesn't start a session or take up a slot.
# app.R wraps its page with load_balancer_ui() to make this work.

lb_service_name <- function() {
  nm <- Sys.getenv("RAILWAY_SERVICE_NAME", "")
  if (nzchar(nm)) nm else "this service"
}

lb_status_self <- function() {
  list(
    service          = lb_service_name(),
    users            = lb_state$active,
    max_users        = if (load_balancer_enabled()) lb_max_users else "no limit (MAX_USERS not set)",
    idle_timeout_min = if (load_balancer_enabled()) lb_idle_timeout_min else "off",
    # next_service     = if (nzchar(lb_next_url)) lb_next_url else "none (last service)"
    routing          = if (length(lb_service_urls) > 0) "service list (SERVICE_URLS)"
                       else if (nzchar(lb_next_url)) paste("next service:", lb_next_url)
                       else "none (last service)",
    url              = if (nzchar(lb_self_url())) lb_self_url() else "unknown",
    # What this service sees of the message to users (site_messages.R)
    site_message     = if (exists("site_message_status")) site_message_status() else "not loaded"
  )
}

# With SERVICE_URLS: every service in the list, in order (each asked for
# ?status=self, so there is no chain to follow)
lb_status_list <- function() {
  lapply(lb_service_urls, function(u) {
    if (lb_is_self(u)) return(c(lb_status_self(), online = TRUE))
    sep <- if (grepl("?", u, fixed = TRUE)) "&" else "?"
    tryCatch({
      r <- curl::curl_fetch_memory(paste0(u, sep, "status=self"),
                                   handle = curl::new_handle(timeout = 4, connecttimeout = 3))
      if (r$status_code != 200) stop("HTTP ", r$status_code)
      s <- jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE)$services[[1]]
      if (is.null(s$users)) stop("not a status response")
      c(s, online = TRUE)
    }, error = function(e) list(url = u, online = FALSE, error = conditionMessage(e)))
  })
}

# This copy's status, then the next copy's (and so on). `hops` stops a
# misconfigured chain (copies pointing at each other) from looping.
lb_status_chain <- function(hops = 0L) {
  out <- list(lb_status_self())
  if (!nzchar(lb_next_url) || hops >= lb_max_hops) return(out)
  sep <- if (grepl("?", lb_next_url, fixed = TRUE)) "&" else "?"
  url <- paste0(lb_next_url, sep, "status&status_hops=", hops + 1L)
  nxt <- tryCatch({
    res <- curl::curl_fetch_memory(url, handle = curl::new_handle(timeout = 10))
    if (res$status_code != 200) stop("HTTP ", res$status_code)
    jsonlite::fromJSON(rawToChar(res$content), simplifyVector = FALSE)$services
  }, error = function(e) {
    list(list(service = lb_next_url, error = paste("could not be reached:", conditionMessage(e))))
  })
  c(out, nxt)
}

lb_status_response <- function(req) {
  query <- parseQueryString(req$QUERY_STRING %||% "")
  hops  <- suppressWarnings(as.integer(query$status_hops))
  if (length(hops) == 0 || is.na(hops)) hops <- 0L
  # services <- lb_status_chain(hops)
  services <- if (identical(query$status, "self")) list(lb_status_self())
              else if (length(lb_service_urls) > 0) lb_status_list()
              else lb_status_chain(hops)
  users <- vapply(services, function(s) if (is.numeric(s$users)) s$users else 0, numeric(1))
  body <- jsonlite::toJSON(list(total_users = sum(users), services = services),
                           auto_unbox = TRUE, pretty = TRUE)
  # Access-Control-Allow-Origin: lets a landing page on another site (e.g.
  # one that routes around a service that is offline) read this from the
  # visitor's browser. The status only contains user counts and URLs.
  httpResponse(200, content_type = "application/json; charset=UTF-8", content = as.character(body),
               headers = list("Access-Control-Allow-Origin" = "*", "Cache-Control" = "no-store"))
}

# Wraps the app's page: ?status gets the status, anything else the app.
load_balancer_ui <- function(page) {
  force(page)
  function(req) {
    query <- parseQueryString(req$QUERY_STRING %||% "")
    if (!is.null(query$status)) return(lb_status_response(req))
    page
  }
}
