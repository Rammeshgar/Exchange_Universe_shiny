root <- getwd()
source(file.path(root, "scripts", "dependency-check.R"))
for (file in c("config.R", "provider.R", "analytics.R", "data.R", "charts.R", "ui.R", "store.R")) source(file.path(root, "R", file))
for (path in c(list.files("R", pattern = "\\.R$", full.names = TRUE), list.files("scripts", pattern = "\\.R$", full.names = TRUE), "app.R")) parse(path)
rates <- c(EUR = 1, USD = 1.2, HUF = 400)
stopifnot(cross_rate(rates, "EUR", "HUF") == 400,
          cross_rate(rates, "USD", "HUF") == 400 / 1.2,
          cross_rate(rates, "HUF", "EUR") * 400 == 1,
          cross_rate(rates, "USD", "USD") == 1,
          is.na(cross_rate(rates, "XXX", "HUF")))
historical <- expand.grid(date = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03")), currency = c("EUR", "USD", "HUF"), stringsAsFactors = FALSE)
historical$rate <- c(1, 1, 1, 2, 1.8, 1.6, 400, 390, 380)
historical$reference <- "EUR"
cmp <- prepare_comparison(historical, "EUR", c("USD", "HUF"), as.Date("2026-01-01"), as.Date("2026-01-03"))
stopifnot(abs(cmp$summary$change[cmp$summary$currency == "USD"] - 25) < 1e-10,
          cmp$data$index[cmp$data$currency == "USD" & cmp$data$date == as.Date("2026-01-03")] == 125)
stopifnot(all(abs(cmp$data$index - cmp$data$performance - 100) < 1e-10))
for (metric in c("performance", "index", "rate")) {
  chart <- plotly::plotly_build(strength_chart_3d(cmp$data, c("USD", "HUF"), "EUR", metric = metric))
  for (trace in chart$x$data) stopifnot(all(abs(trace$z - cmp$data[[metric]][cmp$data$currency == trace$name]) < 1e-10))
}
inverted <- prepare_comparison(historical, "USD", "EUR", as.Date("2026-01-01"), as.Date("2026-01-03"))
stopifnot(abs(inverted$summary$change + 20) < 1e-10)
gapped <- historical[!(historical$currency == "HUF" & historical$date == as.Date("2026-01-02")), ]
gapcmp <- prepare_comparison(gapped, "EUR", c("USD", "HUF"), as.Date("2026-01-01"), as.Date("2026-01-03"))
stopifnot(nrow(gapcmp$data[gapcmp$data$currency == "HUF", ]) == 2,
          all(gapcmp$data$rate > 0))
short <- historical[!(historical$currency == "HUF" & historical$date == as.Date("2026-01-01")), ]
aligned <- prepare_comparison(short, "EUR", c("USD", "HUF"), as.Date("2026-01-01"), as.Date("2026-01-03"))
stopifnot(aligned$start == as.Date("2026-01-02"), all(aligned$summary$start == as.Date("2026-01-02")))
missing <- prepare_comparison(historical, "EUR", c("USD", "XXX"), as.Date("2026-01-01"), as.Date("2026-01-03"))
stopifnot("XXX" %in% missing$missing, !anyNA(missing$data$rate))
history_detail <- detailed_history(gapcmp$data, "EUR", c(USD = "Dollar", HUF = "Forint"))
stopifnot(all(is.na(history_detail$`Daily strength %`[history_detail$Currency == "HUF"])),
  abs(history_detail$`Daily strength %`[history_detail$Currency == "USD" & history_detail$Date == "2026-01-03"] - 12.5) < 1e-10)
stats <- cmp$summary
stats$observed_rate <- stats$rate; stats$observed_change <- stats$change
detail <- detailed_summary(stats, cmp$data, c(USD = "Dollar", HUF = "Forint"))
stopifnot(detail$`Start-date rate`[detail$Currency == "USD"] == 2,
  detail$`Low date`[detail$Currency == "USD"] == "2026-01-03",
  abs(detail$`Inverse rate`[detail$Currency == "USD"] - .625) < 1e-10)
# An earlier observation changes the quote and strength, not full-period extrema.
stats$observed_rate <- c(USD = 1.8, HUF = 390)[stats$currency]
stats$observed_change <- (c(USD = 2, HUF = 400)[stats$currency] / stats$observed_rate - 1) * 100
snapshot <- list(date = "2026-01-03", timestamp = 1767441600)
earlier <- detailed_summary(stats, cmp$data, c(USD = "Dollar", HUF = "Forint"), as.Date("2026-01-02"), snapshot, "EUR")
stopifnot(all(earlier$Base == "EUR"), all(earlier$`Observed date` == "2026-01-02"),
  all(earlier$`Observation type` == "Daily history"), all(earlier$`Source timestamp UTC` == ""),
  all(earlier$`Period low` == detail$`Period low`), all(earlier$`Period high` == detail$`Period high`),
  all(abs(earlier$`Inverse rate` * earlier$`Observed rate` - 1) < 1e-10),
  !"Index 100" %in% names(earlier), !"Index 100" %in% names(history_detail))
stopifnot(effective_refresh(list(interval = 3600), list(limit = 100, remaining = 92)) == 86400,
          effective_refresh(list(interval = 3600), list(limit = 10000, remaining = 9900)) == 3600)
invalid <- tryCatch(validate_rate_vector(list(USD = -2), "BAD!"), exchange_error = function(e) e)
stopifnot(inherits(invalid, "exchange_error"))
mock_response <- function(status = 200L, body = '{"success":true,"rates":{"EUR":1,"USD":1.2},"base":"EUR"}') {
  structure(list(status_code = status, headers = list(`content-type` = "application/json"),
    content = charToRaw(body), url = "https://provider.invalid/latest"), class = "response")
}
test_config <- list(key = "test-placeholder", url = "https://provider.invalid", timeout = 1)
expected <- c(`401` = "credentials", `403` = "plan", `429` = "quota", `503` = "provider")
for (code in names(expected)) {
  error <- tryCatch(api_fetch(test_config, "latest", attempts = 1,
    transport = function(...) mock_response(as.integer(code))), exchange_error = function(e) e)
  stopifnot(inherits(error, "exchange_error"), error$kind == expected[[code]])
}
network <- tryCatch(api_fetch(test_config, "latest", attempts = 1,
  transport = function(...) stop("offline")), exchange_error = function(e) e)
malformed <- tryCatch(api_fetch(test_config, "latest", attempts = 1,
  transport = function(...) mock_response(body = "not-json")), exchange_error = function(e) e)
stopifnot(network$kind == "network", malformed$kind == "payload",
  is.infinite(retry_deadline("credentials")), is.infinite(retry_deadline("plan")),
  retry_deadline("network", 1, now = 1000) == 1300,
  retry_deadline("network", 4, now = 1000) == 3400,
  retry_deadline("quota", now = as.numeric(as.POSIXct("2026-10-07", tz = "UTC"))) == as.numeric(as.POSIXct("2026-11-01", tz = "UTC")))
saved <- jsonlite::fromJSON('[{"id":"one","name":"Example","state":{"base":"EUR","currencies":["USD","HUF"]}}]', simplifyVector = FALSE)
stopifnot(saved[[1]]$id == "one", identical(unlist(saved[[1]]$state$currencies), c("USD", "HUF")))
country <- read.csv("data/countries.csv", encoding = "UTF-8")
stopifnot(!any(grepl("<U\\+", country$name)), !any(grepl("<U\\+", country$currency_name)),
          identical(enc2utf8(country$name[country$iso3 == "ALA"]), "\u00c5land Islands"))
stopifnot(country$currency[country$iso3 == "HUN"] == "HUF", all(country$currency[country$iso3 == "BGR"] == "EUR"),
          "DEU" %in% country$iso3[country$currency == "EUR"], "FRA" %in% country$iso3[country$currency == "EUR"])
world <- readRDS("data/world.rds")
stopifnot(inherits(world, "sf"), nrow(world) > 200, all(c("HUN", "FRA", "USA", "JPN") %in% world$iso3))
color_groups <- list(c("EUR", "USD", "HUF", "GBP", "CNY", "CAD", "JPY"),
  c("AUD", "BRL", "CNY", "CAD", "INR", "ZAR", "KRW"), c("CHF", "EUR", "GBP", "HUF", "USD", "JPY", "XAU"))
for (group in color_groups) for (dark in c(FALSE, TRUE)) {
  colors <- vapply(group, currency_color, character(1), dark = dark, codes = group)
  reverse_colors <- vapply(group, currency_color, character(1), dark = dark, codes = rev(group))
  stopifnot(length(unique(colors)) == length(group), identical(colors, reverse_colors))
}
stopifnot(performance_color(1, TRUE) == currency_color("USD", TRUE),
          performance_color(-1, TRUE) == currency_color("HUF", TRUE),
          performance_color(0) == "#ADB5B0", performance_color(NA_real_) == "#D7D9D2")
cat("PASS: quote math, aligned dates, missing data, quota cadence, invalid credentials, plan restrictions, quota errors, network failure, malformed payload, retry backoff, saved-view encoding, geography and distinct order-independent comparison colors.\n")
