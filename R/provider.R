`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

api_failure <- function(kind, message, status = NA_integer_) {
  structure(list(message = message, call = NULL, kind = kind, status = status),
            class = c("exchange_error", "error", "condition"))
}

provider_transport <- function(config, endpoint, query) {
  httr::GET(paste0(config$url, "/", endpoint), httr::add_headers(apikey = config$key),
            httr::timeout(config$timeout), query = query)
}

api_fetch <- function(config, endpoint, query = list(), attempts = 2L, transport = provider_transport) {
  if (!nzchar(config$key)) stop(api_failure("credentials", "Add your API key to the server environment or .env, then restart the app."))
  for (attempt in seq_len(attempts)) {
    response <- tryCatch(transport(config, endpoint, query), error = function(e) NULL)
    if (is.null(response)) {
      if (attempt < attempts) { Sys.sleep(0.5); next }
      stop(api_failure("network", "The rate provider could not be reached. Your last saved rates are still available."))
    }
    status <- httr::status_code(response)
    if (status >= 500 && attempt < attempts) { Sys.sleep(0.5); next }
    if (status == 401) stop(api_failure("credentials", "The provider rejected the API key. Check the active Exchange Rates Data API subscription.", status))
    if (status == 429) stop(api_failure("quota", "The provider's request allowance has been reached. Saved rates remain available until the quota resets.", status))
    if (status == 403) stop(api_failure("plan", "This endpoint is not included in the active API subscription.", status))
    if (status != 200) stop(api_failure("provider", paste("The rate provider returned HTTP", status, "— please retry later."), status))
    data <- tryCatch(jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"),
                        simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(data)) stop(api_failure("payload", "The provider returned an unreadable response. Saved data has been kept."))
    if (!isTRUE(data$success)) {
      code <- as.character(data$error$code %||% "unknown")
      kind <- if (code %in% c("101", "102")) "credentials" else if (code %in% c("104")) "quota" else if (code %in% c("105", "106")) "plan" else "provider"
      stop(api_failure(kind, paste0("The provider could not supply these rates (code ", code, "). Check the subscription's endpoint access.")))
    }
    headers <- httr::headers(response)
    number_header <- function(name) suppressWarnings(as.numeric(headers[[name]] %||% NA))
    return(list(payload = data, fetched_at = as.numeric(Sys.time()),
                quota = list(limit = number_header("x-ratelimit-limit-month"),
                             remaining = number_header("x-ratelimit-remaining-month"))))
  }
}

validate_rate_vector <- function(rates, base) {
  values <- unlist(rates, use.names = TRUE)
  values <- suppressWarnings(setNames(as.numeric(values), names(values)))
  values <- values[grepl("^[A-Z]{3}$", names(values)) & is.finite(values) & values > 0]
  if (!length(values) || !is.character(base) || !grepl("^[A-Z]{3}$", base))
    stop(api_failure("payload", "The response did not contain valid positive exchange rates."))
  values[base] <- 1
  values
}

fetch_latest <- function(config) {
  response <- api_fetch(config, "latest")
  data <- response$payload
  rates <- validate_rate_vector(data$rates, data$base)
  timestamp <- suppressWarnings(as.numeric(data$timestamp))
  if (length(timestamp) != 1 || !is.finite(timestamp)) stop(api_failure("payload", "The provider omitted the observation timestamp."))
  if (timestamp > as.numeric(Sys.time()) + 300) stop(api_failure("payload", "The provider returned a future observation timestamp."))
  list(rates = rates, base = data$base, timestamp = timestamp, date = as.character(data$date),
       fetched_at = response$fetched_at, quota = response$quota, source = "APILayer")
}

fetch_symbols <- function(config) {
  response <- api_fetch(config, "symbols")
  names <- unlist(response$payload$symbols, use.names = TRUE)
  if (!length(names)) stop(api_failure("payload", "The provider returned no currency names."))
  list(symbols = names, fetched_at = response$fetched_at, quota = response$quota)
}

fetch_history <- function(config, start, end) {
  start <- as.Date(start); end <- as.Date(end)
  if (length(start) != 1 || length(end) != 1 || is.na(start) || is.na(end) ||
      start < as.Date("1999-01-01") || start > end || as.integer(end - start) > 364 || end >= Sys.Date())
    stop(api_failure("range", "Choose dates from 1 January 1999, spanning at most 365 days and ending before today."))
  response <- api_fetch(config, "timeseries", list(start_date = as.character(start), end_date = as.character(end)))
  data <- response$payload
  days <- names(data$rates)
  if (!length(days) || is.null(data$base)) stop(api_failure("payload", "No historical observations were returned."))
  result <- lapply(days, function(day) {
    if (is.na(as.Date(day))) return(NULL)
    rates <- validate_rate_vector(data$rates[[day]], data$base)
    data.frame(date = as.Date(day), currency = names(rates), rate = unname(rates),
               reference = data$base, stringsAsFactors = FALSE)
  })
  result <- do.call(rbind, result)
  result <- result[result$date >= start & result$date <= end, ]
  if (!nrow(result)) stop(api_failure("payload", "The returned observations did not match your date range."))
  list(data = result, fetched_at = response$fetched_at, quota = response$quota, source = "APILayer")
}

atomic_rds <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile(pattern = "exchange-", tmpdir = dirname(path))
  on.exit(unlink(temp), add = TRUE)
  saveRDS(value, temp, compress = "gzip")
  if (.Platform$OS.type == "windows" && file.exists(path)) {
    backup <- paste0(path, ".previous")
    if (file.exists(backup)) unlink(backup)
    file.rename(path, backup)
    if (!file.rename(temp, path)) { file.rename(backup, path); stop("Cache write failed.") }
    unlink(backup)
  } else if (!file.rename(temp, path)) stop("Cache write failed.")
  invisible(path)
}

safe_read_rds <- function(path, default = NULL) {
  if (!file.exists(path)) return(default)
  tryCatch(readRDS(path), error = function(e) default)
}

effective_refresh <- function(config, quota) {
  # Preserve a metadata/history allowance on small plans. A paid quota can
  # immediately restore the requested hourly cadence without changing code.
  limit <- quota$limit %||% NA_real_
  remaining <- quota$remaining %||% NA_real_
  seconds <- config$interval
  if (is.finite(limit) && limit < 1000) seconds <- max(seconds, 86400)
  if (is.finite(remaining) && remaining <= 10) seconds <- max(seconds, 7 * 86400)
  seconds
}

retry_deadline <- function(kind, failures = 1L, now = as.numeric(Sys.time())) {
  if (kind %in% c("credentials", "plan")) return(Inf)
  if (identical(kind, "quota")) {
    current <- as.Date(as.POSIXct(now, origin = "1970-01-01", tz = "UTC"))
    next_month <- seq(as.Date(format(current, "%Y-%m-01")), length.out = 2, by = "month")[2]
    return(as.numeric(as.POSIXct(next_month, tz = "UTC")))
  }
  now + min(3600, 300 * 2 ^ max(0, min(4, failures - 1)))
}

run_provider_job <- function(root, spec, libpaths) {
  .libPaths(libpaths)
  source(file.path(root, "R", "config.R"), local = TRUE)
  source(file.path(root, "R", "provider.R"), local = TRUE)
  config <- load_exchange_config(root)
  result <- tryCatch({
    value <- switch(spec$type, latest = fetch_latest(config), symbols = fetch_symbols(config),
                    history = fetch_history(config, spec$start, spec$end))
    list(ok = TRUE, type = spec$type, value = value)
  }, exchange_error = function(e) list(ok = FALSE, type = spec$type, kind = e$kind, message = e$message),
     error = function(e) list(ok = FALSE, type = spec$type, kind = "internal", message = "The rate request could not be completed. Please retry."))
  result
}
