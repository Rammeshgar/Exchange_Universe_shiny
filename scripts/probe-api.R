args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[1] else getwd()
source(file.path(root, "R", "config.R"))
config <- load_exchange_config(root)
if (!nzchar(config$key)) stop("No API key found in the environment or .env.")
for (endpoint in c("symbols", "latest", "timeseries")) {
  query <- if (endpoint == "timeseries") list(start_date = as.character(Sys.Date() - 30),
           end_date = as.character(Sys.Date() - 1), symbols = "USD,HUF,GBP,EUR") else list()
  tryCatch({
    res <- httr::GET(paste0(config$url, "/", endpoint), httr::add_headers(apikey = config$key),
                     httr::timeout(config$timeout), query = query)
    payload <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    cat(endpoint, "HTTP", httr::status_code(res), "success", isTRUE(payload$success), "\n")
    if (endpoint == "symbols") cat("Symbol count:", length(payload$symbols), "\n")
    if (endpoint == "latest") cat("Rate count:", length(payload$rates), "base:", payload$base,
      "observation:", payload$date, "timestamp:", payload$timestamp, "\n")
    if (endpoint == "timeseries") cat("History dates:", length(payload$rates), "\n")
    h <- httr::headers(res)
    for (name in c("x-ratelimit-limit-month", "x-ratelimit-remaining-month"))
      if (!is.null(h[[name]])) cat(name, h[[name]], "\n")
    # Only print known non-secret fields. Never print request headers or raw errors.
    if (!isTRUE(payload$success)) cat("Error code:", as.character(payload$error$code %||% ""), "\n")
  }, error = function(e) cat(endpoint, "could not complete the request.\n"))
}
