read_exchange_env <- function(path) {
  if (!file.exists(path)) return(invisible(FALSE))
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  for (line in lines) {
    line <- trimws(sub("^\ufeff", "", line))
    if (!grepl("^[A-Za-z_][A-Za-z0-9_]*\\s*=", line)) next
    name <- trimws(sub("=.*$", "", line))
    value <- trimws(sub("^[^=]*=", "", line))
    value <- sub("^(['\"])(.*)\\1$", "\\2", value)
    if (!nzchar(Sys.getenv(name))) do.call(Sys.setenv, setNames(list(value), name))
  }
  invisible(TRUE)
}

load_exchange_config <- function(root = getwd()) {
  # A deployment environment takes precedence over local dotenv values.
  paths <- unique(c(
    Sys.getenv("EXCHANGE_ENV_FILE"),
    file.path(root, ".env")
  ))
  for (path in paths[nzchar(paths)]) read_exchange_env(path)
  key <- Sys.getenv("APILAYER_API_KEY", Sys.getenv("API_key", Sys.getenv("API_KEY")))
  cache <- Sys.getenv("EXCHANGE_CACHE_DIR", file.path(root, "rate-cache"))
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  interval <- suppressWarnings(as.numeric(Sys.getenv("EXCHANGE_REFRESH_SECONDS", "3600")))
  if (!is.finite(interval) || interval < 3600) interval <- 3600
  list(key = trimws(key), cache = normalizePath(cache, winslash = "/", mustWork = TRUE),
       interval = interval, url = "https://api.apilayer.com/exchangerates_data",
       max_days = 365L, timeout = 25L)
}
