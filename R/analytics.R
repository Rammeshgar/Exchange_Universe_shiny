cross_rate <- function(rates, from, to) {
  denominator <- unname(rates[from]); numerator <- unname(rates[to])
  if (length(denominator) != 1 || length(numerator) != 1 || !is.finite(denominator) ||
      !is.finite(numerator) || denominator <= 0 || numerator <= 0) return(NA_real_)
  numerator / denominator
}

rebase_history <- function(history, base, currencies) {
  empty <- data.frame(date = as.Date(character()), currency = character(), rate = numeric())
  if (is.null(history) || !nrow(history)) return(empty)
  bases <- history[history$currency == base, c("date", "reference", "rate")]
  names(bases)[3] <- "denominator"
  selected <- history[history$currency %in% currencies, ]
  aligned <- merge(selected, bases, by = c("date", "reference"), all.x = TRUE)
  aligned$rate <- aligned$rate / aligned$denominator
  aligned <- aligned[is.finite(aligned$rate) & aligned$rate > 0, c("date", "currency", "rate")]
  aligned[order(aligned$date, aligned$currency), ]
}

prepare_comparison <- function(history, base, currencies, start, end) {
  data <- rebase_history(history[history$date >= start & history$date <= end, ], base, currencies)
  available <- unique(data$currency)
  if (!nrow(data)) return(list(data = data, summary = data.frame(), aligned = FALSE, missing = currencies))
  # All series use the same first/last observed dates; unavailable currencies
  # are disclosed rather than silently shifted to a different starting date.
  dates_by_currency <- split(as.character(data$date), data$currency)
  common <- Reduce(intersect, dates_by_currency)
  if (!length(common)) return(list(data = data[FALSE, ], summary = data.frame(), aligned = FALSE, missing = currencies))
  first <- as.Date(min(common)); last <- as.Date(max(common))
  data <- data[data$date >= first & data$date <= last, ]
  data$performance <- NA_real_; data$index <- NA_real_
  summaries <- lapply(currencies[currencies %in% available], function(code) {
    idx <- which(data$currency == code)
    series <- data[idx, ]
    opening <- series$rate[series$date == first][1]
    data$performance[idx] <<- (opening / series$rate - 1) * 100
    data$index[idx] <<- opening / series$rate * 100
    closing <- series$rate[series$date == last][1]
    data.frame(currency = code, rate = closing, change = (opening / closing - 1) * 100,
      low = min(series$rate), high = max(series$rate), low_date = series$date[which.min(series$rate)],
      high_date = series$date[which.max(series$rate)], start = first, end = last, observations = nrow(series))
  })
  list(data = data, summary = do.call(rbind, summaries), aligned = TRUE,
       missing = setdiff(currencies, available), start = first, end = last)
}

format_rate <- function(value) {
  if (!length(value) || !is.finite(value)) return("Unavailable")
  digits <- if (abs(value) >= 1000) 2 else if (abs(value) >= 1) 4 else 6
  formatC(value, digits = digits, format = "f", big.mark = ",")
}

currency_color <- function(code, dark = FALSE, codes = NULL) {
  primary <- c(EUR = "#B06314", USD = "#267E77", HUF = "#A65530", GBP = "#6B63AE", JPY = "#397AA8", CHF = "#B3446C")
  palette <- c("#267E77", "#6B63AE", "#A65530", "#397AA8", "#B3446C", "#527837", "#B06314", "#3D6975")
  color <- if (code %in% names(primary)) unname(primary[code]) else palette[(sum(utf8ToInt(code) * c(1, 7, 13)) %% length(palette)) + 1]
  if (!is.null(codes) && code %in% codes) {
    # Reserve familiar colors, then resolve hash collisions for this comparison.
    # Sorting makes colors independent of selection order. Include the base in
    # every caller so charts, cards and geographic highlights share one mapping.
    codes <- sort(unique(codes[nzchar(codes)]))
    assigned <- primary[intersect(names(primary), codes)]
    for (other in setdiff(codes, names(assigned))) {
      preferred <- currency_color(other)
      available <- setdiff(palette, unname(assigned))
      assigned[other] <- if (preferred %in% available || !length(available)) preferred else available[1]
    }
    color <- unname(assigned[code])
  }
  if (dark) {
    rgb <- grDevices::col2rgb(color) / 255
    color <- grDevices::rgb(rgb[1] * .6 + .4, rgb[2] * .6 + .4, rgb[3] * .6 + .4)
  }
  color
}

performance_color <- function(value, dark = FALSE) {
  if (!is.finite(value)) return("#D7D9D2")
  if (abs(value) < 0.01) return("#ADB5B0")
  if (dark) return(currency_color(if (value > 0) "USD" else "HUF", TRUE))
  if (value > 0) "#267E77" else "#A65530"
}
