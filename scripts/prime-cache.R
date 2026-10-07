args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(if (length(args)) args[1] else getwd(), winslash = "/")
source(file.path(root, "R", "config.R"))
source(file.path(root, "R", "provider.R"))
config <- load_exchange_config(root)
for (type in c("symbols", "latest", "history")) {
  path <- file.path(config$cache, paste0(type, ".rds"))
  if (file.exists(path)) { cat(type, "already cached\n"); next }
  value <- switch(type, symbols = fetch_symbols(config), latest = fetch_latest(config),
    history = fetch_history(config, Sys.Date() - 365, Sys.Date() - 1))
  atomic_rds(if (type == "history") value$data else value, path)
  atomic_rds(value$quota, file.path(config$cache, "quota.rds"))
  cat(type, "saved; remaining monthly requests:", value$quota$remaining, "\n")
  if (type == "history") cat("Daily observations:", nrow(value$data), "\n")
}
