detailed_summary <- function(summary, data, names, observed_date = NULL, latest = NULL, base = "") {
  if (!nrow(summary)) return(data.frame())
  opening <- vapply(summary$currency, function(code) data$rate[data$currency == code & data$date == summary$start[summary$currency == code]][1], numeric(1))
  dates <- if (is.null(observed_date)) summary$end else rep(as.Date(observed_date), nrow(summary))
  snapshot <- if (is.null(latest)) rep(FALSE, nrow(summary)) else dates == as.Date(latest$date)
  stamp <- if (is.null(latest)) "" else format(as.POSIXct(latest$timestamp, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  data.frame(Currency = summary$currency, Name = unname(names[summary$currency]), Base = base, `Observed date` = as.character(dates),
    `Observed rate` = summary$observed_rate, `Inverse rate` = 1 / summary$observed_rate,
    `Start-date rate` = opening, `Strength %` = summary$observed_change,
    `Period low` = summary$low, `Low date` = as.character(summary$low_date),
    `Period high` = summary$high, `High date` = as.character(summary$high_date),
    Observations = summary$observations, `Period start` = as.character(summary$start),
    `Period end` = as.character(summary$end), `Observation type` = ifelse(snapshot, "Indicative snapshot", "Daily history"),
    `Source timestamp UTC` = ifelse(snapshot, stamp, ""), check.names = FALSE)
}

detailed_history <- function(data, base, names, latest = NULL) {
  if (!nrow(data)) return(data.frame())
  data <- data[order(data$date, data$currency), ]
  daily <- rep(NA_real_, nrow(data))
  for (code in unique(data$currency)) {
    idx <- which(data$currency == code)
    if (length(idx) < 2) next
    previous <- head(idx, -1); current <- tail(idx, -1)
    contiguous <- as.integer(data$date[current] - data$date[previous]) == 1L
    daily[current[contiguous]] <- (data$rate[previous[contiguous]] / data$rate[current[contiguous]] - 1) * 100
  }
  snapshot <- if (is.null(latest)) rep(FALSE, nrow(data)) else data$date == as.Date(latest$date)
  stamp <- if (is.null(latest)) "" else format(as.POSIXct(latest$timestamp, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  data.frame(Date = as.character(data$date), Currency = data$currency, Name = unname(names[data$currency]), Base = base,
    Rate = data$rate, `Inverse rate` = 1 / data$rate, `Strength %` = data$performance,
    `Daily strength %` = daily,
    `Observation type` = ifelse(snapshot, "Indicative snapshot", "Daily history"),
    `Source timestamp UTC` = ifelse(snapshot, stamp, ""), check.names = FALSE)
}

current_quotes <- function(latest, base, names, countries) {
  if (is.null(latest)) return(data.frame())
  codes <- sort(unique(c(names(latest$rates), base)))
  rates <- vapply(codes, function(code) cross_rate(latest$rates, base, code), numeric(1))
  places <- lapply(codes, function(code) unique(countries$name[countries$currency == code]))
  geography <- vapply(places, function(list) {
    if (!length(list)) return("No current country mapping")
    paste0(paste(head(list, 4), collapse = ", "), if (length(list) > 4) paste0(" +", length(list) - 4, " more") else "")
  }, character(1))
  data.frame(Currency = codes, Name = unname(names[codes]), Base = base, Rate = rates, `Inverse rate` = 1 / rates,
    `Mapped places` = lengths(places), `Countries / territories` = geography,
    `Observed UTC` = format(as.POSIXct(latest$timestamp, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S"),
    Type = "Indicative snapshot", check.names = FALSE)
}
