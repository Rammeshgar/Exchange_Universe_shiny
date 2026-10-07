# Schedule this script with your hosting platform's scheduler, using a durable
# EXCHANGE_CACHE_DIR shared with the app. It respects the active plan's quota.
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(if (length(args)) args[1] else getwd(), winslash = "/")
source(file.path(root, "R", "config.R"))
source(file.path(root, "R", "provider.R"))
config <- load_exchange_config(root)
latest <- safe_read_rds(file.path(config$cache, "latest.rds"))
quota <- safe_read_rds(file.path(config$cache, "quota.rds"), list())
if (!is.null(latest) && as.numeric(Sys.time()) - latest$fetched_at < effective_refresh(config, quota)) {
  cat("Current snapshot is within the plan-aware refresh window.\n")
} else {
  value <- fetch_latest(config)
  atomic_rds(value, file.path(config$cache, "latest.rds"))
  atomic_rds(value$quota, file.path(config$cache, "quota.rds"))
  path <- file.path(config$cache, "snapshots", paste0(value$timestamp, ".rds"))
  if (!file.exists(path)) atomic_rds(value, path)
  cat("Collected a verified observation at", format(as.POSIXct(value$timestamp, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M UTC"), "\n")
}
