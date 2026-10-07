new_exchange_store <- function(root, config) {
  values <- shiny::reactiveValues(
    latest = safe_read_rds(file.path(config$cache, "latest.rds")),
    symbols = safe_read_rds(file.path(config$cache, "symbols.rds")),
    history = safe_read_rds(file.path(config$cache, "history.rds"), data.frame()),
    quota = safe_read_rds(file.path(config$cache, "quota.rds"), list()),
    busy = FALSE, activity = "", error = NULL, history_error = NULL, revision = 0L)
  state <- new.env(parent = emptyenv())
  state$queue <- list(); state$process <- NULL; state$active <- NULL
  state$last_attempt <- 0; state$failed_ranges <- character()
  state$retry_after <- c(latest = 0, symbols = 0, history = 0)
  state$failures <- c(latest = 0L, symbols = 0L, history = 0L)
  state$global_block <- 0

  enqueue <- function(spec) {
    if (!spec$type %in% c("latest", "symbols", "history")) return(invisible(FALSE))
    now <- as.numeric(Sys.time())
    if (now < state$global_block || now < state$retry_after[spec$type]) return(invisible(FALSE))
    id <- if (spec$type == "history") paste(spec$type, spec$start, spec$end) else spec$type
    pending <- vapply(state$queue, function(x) x$id, character(1))
    if (id %in% pending || identical(id, state$active$id)) return(invisible(FALSE))
    # Bound aggregate work, including an active history job; reserve capacity
    # for current rates/symbols rather than allowing history to fill the queue.
    history_jobs <- sum(vapply(state$queue, function(x) identical(x$type, "history"), logical(1))) +
      as.integer(identical(state$active$type, "history"))
    if (length(state$queue) >= 8L || (spec$type == "history" && history_jobs >= 3L)) return(invisible(FALSE))
    spec$id <- id
    state$queue[[length(state$queue) + 1L]] <- spec
    invisible(TRUE)
  }

  save_result <- function(result) {
    if (!result$ok) {
      kind <- result$kind %||% "internal"
      state$failures[result$type] <- state$failures[result$type] + 1L
      state$retry_after[result$type] <- retry_deadline(kind, state$failures[result$type])
      state$queue <- Filter(function(job) !identical(job$type, result$type), state$queue)
      if (kind %in% c("credentials", "quota")) {
        state$global_block <- state$retry_after[result$type]
        state$queue <- list()
      }
      if (result$type == "history") {
        values$history_error <- result$message
        state$failed_ranges <- unique(c(state$failed_ranges, state$active$id))
      } else values$error <- result$message
      return(invisible(NULL))
    }
    value <- result$value
    state$failures[result$type] <- 0L; state$retry_after[result$type] <- 0
    values$quota <- value$quota
    atomic_rds(value$quota, file.path(config$cache, "quota.rds"))
    if (result$type == "latest") {
      values$latest <- value; values$error <- NULL
      atomic_rds(value, file.path(config$cache, "latest.rds"))
      dir.create(file.path(config$cache, "snapshots"), showWarnings = FALSE)
      path <- file.path(config$cache, "snapshots", paste0(value$timestamp, ".rds"))
      if (!file.exists(path)) atomic_rds(value, path)
    } else if (result$type == "symbols") {
      values$symbols <- value
      atomic_rds(value, file.path(config$cache, "symbols.rds"))
    } else if (result$type == "history") {
      existing <- values$history
      combined <- if (nrow(existing)) rbind(existing, value$data) else value$data
      combined <- combined[!duplicated(combined[c("date", "currency", "reference")], fromLast = TRUE), ]
      values$history <- combined[order(combined$date, combined$currency), ]
      values$history_error <- NULL
      atomic_rds(values$history, file.path(config$cache, "history.rds"))
    }
    values$revision <- values$revision + 1L
  }

  tick <- shiny::observe({
    shiny::invalidateLater(if (is.null(state$process)) 3000 else 250, session = NULL)
    shiny::isolate({
      now <- as.numeric(Sys.time())
      latest <- values$latest
      interval <- effective_refresh(config, values$quota)
      if (!is.null(state$process) && !state$process$is_alive()) {
        result <- tryCatch(state$process$get_result(), error = function(e)
          list(ok = FALSE, type = state$active$type, message = "The background data request stopped. Please retry."))
        tryCatch(save_result(result), error = function(e) { values$error <- "Rates were retrieved, but the local cache could not be saved." })
        state$process <- NULL; state$active <- NULL
      }
      if (now >= state$global_block) {
        if ((is.null(latest) || now - latest$fetched_at >= interval) &&
            now - state$last_attempt >= 300 && now >= state$retry_after["latest"]) {
          if (enqueue(list(type = "latest"))) state$last_attempt <- now
        }
        if (is.null(values$symbols) && now >= state$retry_after["symbols"]) enqueue(list(type = "symbols"))
      }
      eligible <- if (now < state$global_block) integer() else which(vapply(state$queue,
        function(job) now >= state$retry_after[job$type], logical(1)))
      if (is.null(state$process) && length(eligible)) {
        next_job <- eligible[1]
        spec <- state$queue[[next_job]]; state$queue <- state$queue[-next_job]
        state$active <- spec
        values$activity <- switch(spec$type, latest = "Checking current rates…", symbols = "Loading currency names…", history = "Loading daily history…")
        state$process <- callr::r_bg(run_provider_job,
          args = list(root = root, spec = spec, libpaths = .libPaths()),
          libpath = .libPaths(), stdout = "|", stderr = "|", supervise = TRUE)
      }
      values$busy <- !is.null(state$process) || length(state$queue) > 0
      if (!values$busy) values$activity <- ""
    })
  }, domain = NULL)

  ensure_history <- function(start, end, retry = FALSE) {
    start <- tryCatch(as.Date(start), error = function(e) as.Date(NA))
    end <- tryCatch(as.Date(end), error = function(e) as.Date(NA))
    if (length(start) != 1 || length(end) != 1 || is.na(start) || is.na(end) ||
        start < as.Date("1999-01-01") || end > Sys.Date()) return(invisible(FALSE))
    end <- min(end, Sys.Date() - 1)
    if (start > end || as.integer(end - start) > 364) return(invisible(FALSE))
    history <- shiny::isolate(values$history)
    have <- if (nrow(history)) sort(unique(history$date)) else as.Date(character())
    needed <- seq(start, end, by = "day")
    missing <- needed[!needed %in% have]
    if (!length(missing)) return(invisible(TRUE))
    id <- paste("history", min(missing), max(missing))
    if (id %in% state$failed_ranges && !retry) return(invisible(FALSE))
    now <- as.numeric(Sys.time())
    # Retry may revisit a failed range only after the shared cooldown expires.
    # Credential/plan changes require an operator restart, never a visitor reset.
    if (now < state$global_block || now < state$retry_after["history"]) return(invisible(FALSE))
    enqueue(list(type = "history", start = as.character(min(missing)), end = as.character(max(missing))))
  }

  refresh <- function() {
    # Repeated visitor clicks reuse the most recent five-minute check.
    latest <- shiny::isolate(values$latest)
    now <- as.numeric(Sys.time())
    if (now < state$global_block || now < state$retry_after["latest"] ||
        (!is.null(latest) && now - latest$fetched_at < 300) || now - state$last_attempt < 300) return(FALSE)
    if (is.null(shiny::isolate(values$symbols))) enqueue(list(type = "symbols"))
    accepted <- enqueue(list(type = "latest"))
    if (accepted) state$last_attempt <- now
    accepted
  }
  list(values = values, ensure_history = ensure_history, refresh = refresh,
       shutdown = function() { tick$destroy(); if (!is.null(state$process)) state$process$kill() })
}
