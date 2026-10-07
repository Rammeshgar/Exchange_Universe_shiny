options(shiny.sanitize.errors = TRUE)
needed <- c("shiny", "bslib", "leaflet", "sf", "echarts4r", "DT", "httr", "jsonlite", "htmlwidgets", "callr", "plotly")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop(paste("Install the app dependencies first:", paste(missing, collapse = ", ")))
library(shiny)
app_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
for (file in c("config.R", "provider.R", "analytics.R", "data.R", "store.R", "charts.R", "ui.R")) source(file.path(app_root, "R", file), local = TRUE)
config <- load_exchange_config(app_root)
store <- new_exchange_store(app_root, config)
onStop(store$shutdown)
countries <- read.csv(file.path(app_root, "data", "countries.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
world <- readRDS(file.path(app_root, "data", "world.rds"))
sf::sf_use_s2(FALSE)
initial_symbols <- isolate(store$values$symbols)
initial_latest <- isolate(store$values$latest)
catalog <- if (!is.null(initial_symbols)) initial_symbols$symbols else {
  basic <- countries[!duplicated(countries$currency) & nzchar(countries$currency), ]
  setNames(basic$currency_name, basic$currency)
}
if (is.null(initial_symbols)) catalog[c("EUR", "USD", "HUF", "GBP")] <- c("Euro", "United States Dollar", "Hungarian Forint", "British Pound Sterling")
make_choices <- function(symbols) {
  places <- vapply(names(symbols), function(code) {
    names <- unique(countries$name[countries$currency == code])
    if (!length(names)) return("")
    paste(names, collapse = ", ")
  }, character(1))
  setNames(names(symbols), paste0(names(symbols), " · ", symbols,
    ifelse(nzchar(places), paste0(" — ", places), "")))
}
ui <- make_ui(make_choices(catalog))

server <- function(input, output, session) {
  data_requested <- reactiveVal(FALSE)
  observeEvent(input$current_view, {
    if (identical(input$current_view, "data")) data_requested(TRUE)
  })
  output$data_table_slot <- renderUI({
    if (!data_requested()) return(NULL)
    DT::DTOutput("summary_table")
  })
  focused <- reactiveVal(NULL)
  selected_country <- reactiveVal(NULL)
  map_locations <- reactiveVal(list())
  map_added_currencies <- reactiveVal(character())
  restored <- reactiveVal(FALSE)
  saved_views <- reactiveVal(list())
  currency_names <- reactive({ store$values$symbols$symbols %||% catalog })
  available <- reactive(names(currency_names()))
  selected <- reactive({ head(unique(setdiff(input$currencies %||% character(), input$base)), 6) })
  dark <- reactive(identical(input$theme, "dark"))
  color_for <- function(code) currency_color(code, dark(), c(input$base, selected()))
  observation_strength <- function(code) {
    if (identical(code, input$base)) return(0)
    data <- comparison()$data
    values <- data$performance[data$date == observation() & data$currency == code]
    if (length(values)) values[1] else NA_real_
  }
  map_color_for <- function(code) if (identical(input$map_mode, "change")) performance_color(observation_strength(code), dark()) else color_for(code)
  currency_name <- function(code) unname(currency_names()[code]) %||% code
  range <- reactive({
    if (identical(input$period, "custom")) {
      req(input$dates)
      dates <- as.Date(input$dates)
    } else {
      days <- suppressWarnings(as.integer(input$period %||% "30"))
      if (!is.finite(days) || !days %in% c(30, 90, 180, 365)) days <- 30L
      dates <- c(Sys.Date() - days, Sys.Date())
    }
    validate(need(length(dates) == 2 && !anyNA(dates) && dates[1] <= dates[2], "Choose a valid start and end date."),
      need(dates[2] <= Sys.Date(), "Future exchange rates are not available."),
      need(dates[1] >= as.Date("1999-01-01"), "Choose dates from 1 January 1999 onward."),
      need(as.integer(dates[2] - dates[1]) <= 365, "Compare at most one year at a time."))
    dates
  })

  observeEvent(store$values$symbols, {
    choices <- make_choices(currency_names())
    for (id in c("base", "from", "to")) updateSelectizeInput(session, id, choices = choices, selected = isolate(input[[id]]))
    updateSelectizeInput(session, "currencies", choices = choices, selected = isolate(input$currencies))
  }, ignoreInit = TRUE)

  observe({
    dates <- range()
    store$ensure_history(dates[1], dates[2])
  })

  history <- reactive({
    data <- store$values$history
    if (!nrow(data)) data <- data.frame(date = as.Date(character()), currency = character(), rate = numeric(), reference = character())
    latest <- store$values$latest
    if (!is.null(latest) && !is.na(as.Date(latest$date))) {
      today <- data.frame(date = as.Date(latest$date), currency = names(latest$rates), rate = unname(latest$rates), reference = latest$base)
      # A cached intraday snapshot supplements only its real observation day.
      data <- data[data$date != as.Date(latest$date), ]
      data <- rbind(data, today)
    }
    data
  })
  comparison <- reactive({
    dates <- range()
    req(input$base)
    prepare_comparison(history(), input$base, selected(), dates[1], dates[2])
  })
  observation <- reactive({
    cmp <- comparison()
    if (!nrow(cmp$data)) return(as.Date(NA))
    chosen <- suppressWarnings(as.Date(input$observation %||% ""))
    dates <- sort(unique(cmp$data$date))
    if (length(chosen) != 1 || is.na(chosen) || !chosen %in% dates) max(dates) else chosen
  })
  period_summary <- reactive({
    cmp <- comparison()
    if (!nrow(cmp$summary)) return(cmp$summary)
    values <- cmp$data[cmp$data$date == observation(), ]
    summary <- cmp$summary
    summary$observed_rate <- values$rate[match(summary$currency, values$currency)]
    summary$observed_change <- values$performance[match(summary$currency, values$currency)]
    summary
  })

  output$period_label <- renderUI({
    dates <- range()
    tags$div(class = "period-label", paste(format(dates[1], "%d %b %Y"), "—", format(dates[2], "%d %b %Y")))
  })
  output$header_status <- renderUI({
    latest <- store$values$latest
    stale <- !is.null(latest) && as.numeric(Sys.time()) - latest$timestamp > 48 * 3600
    status <- if (store$values$busy) store$values$activity else if (is.null(latest)) "Waiting for rates" else
      if (!is.null(store$values$error)) "Cached rates · check failed" else if (stale) "Older observation" else "Rates ready"
    stamp <- if (is.null(latest)) "Waiting for a provider observation" else paste("Provider observation:",
      format(as.POSIXct(latest$timestamp, origin = "1970-01-01", tz = "UTC"), "%d %b %Y, %H:%M UTC"), "· indicative, not a live trading quote")
    tags$span(class = "header-status", role = "status", title = stamp,
      tags$span(class = paste("status-dot", if (is.null(latest) || !is.null(store$values$error) || stale) "warning" else ""), `aria-hidden` = "true"),
      tags$span(status))
  })
  output$data_notice <- renderUI({
    if (!is.null(store$values$error)) return(tags$div(class = "data-notice warning", role = "status", eu_icon("info"),
      tags$span(store$values$error), icon_action("retry_rates", "Retry", "refresh")))
    if (is.null(store$values$latest)) return(tags$div(class = "data-notice", role = "status", eu_icon("refresh"), "Loading provider rates. You can set up your comparison while the request completes."))
    if (!is.null(store$values$history_error)) return(tags$div(class = "data-notice warning", role = "status", eu_icon("info"),
      store$values$history_error, icon_action("retry_history", "Retry history", "refresh")))
    if (!length(selected())) return(tags$div(class = "data-notice", role = "status", eu_icon("chart"), "Choose at least one comparison currency to populate the charts and table."))
    latest <- store$values$latest
    if (as.numeric(Sys.time()) - latest$timestamp > 48 * 3600) return(tags$div(class = "data-notice warning", eu_icon("info"),
      paste("Showing the last available observation:", format(as.POSIXct(latest$timestamp, origin = "1970-01-01", tz = "UTC"), "%d %b %Y, %H:%M UTC"), ". The market or data source may not have refreshed.")))
    if (store$values$busy) tags$div(class = "data-notice", role = "status", eu_icon("refresh"), store$values$activity)
    else NULL
  })
  observeEvent(input$refresh, {
    queued <- store$refresh()
    session$sendCustomMessage("exchange-toast", if (queued) "Rate check started in the background." else "No new request queued: the shared cooldown, recent check or pending-request limit applies. Saved rates remain available.")
  })
  observeEvent(input$retry_rates, store$refresh())
  observeEvent(input$retry_history, { dates <- range(); store$ensure_history(dates[1], dates[2], retry = TRUE) })

  output$rate_cards <- renderUI({
    codes <- selected()
    if (!length(codes)) return(NULL)
    latest <- store$values$latest
    cmp <- comparison()
    cards <- lapply(codes, function(code) {
      value <- if (is.null(latest)) NA_real_ else cross_rate(latest$rates, input$base, code)
      change <- cmp$summary$change[match(code, cmp$summary$currency)]
      has_change <- length(change) == 1 && is.finite(change)
      tags$article(class = "rate-card", style = paste0("--currency-color:", color_for(code)),
        tags$div(class = "rate-card-top", tags$div(tags$strong(class = "currency-code", tags$i(class = "currency-dot"), code),
          tags$span(class = "currency-name", title = currency_name(code), currency_name(code))),
          tags$div(class = "rate-card-tools", tags$span(class = "pair-label", paste(input$base, "/", code)),
            tags$button(type = "button", class = "rate-info", `data-rate-help` = paste0("rate_help_", code),
              `aria-label` = paste("Explain", code, "quote and period strength"), `aria-describedby` = paste0("rate_help_", code), eu_icon("info", 18)))),
        tags$div(class = "rate-value", format_rate(value)),
        tags$div(class = "rate-card-bottom", tags$span(class = paste("change-pill", if (has_change && change < 0) "negative" else ""),
          if (has_change) sprintf("%+.2f%%", change) else "No history"), tags$span("strength over period")),
        tags$button(type = "button", class = "rate-focus", `data-focus-currency` = code, `aria-label` = paste("Highlight", code, "on the world map")))
    })
    help <- lapply(codes, function(code) {
      value <- if (is.null(latest)) NA_real_ else cross_rate(latest$rates, input$base, code)
      change <- cmp$summary$change[match(code, cmp$summary$currency)]
      has_change <- length(change) == 1 && is.finite(change)
      quote <- if (is.finite(value)) paste0("1 ", input$base, " = ", format_rate(value), " ", code, ".") else "No current quote is available for this pair."
      movement <- if (!has_change) "No aligned history is available for your selected period." else paste0(
        sprintf("%+.2f%%", change), " means ", code, if (abs(change) < .005) " was essentially unchanged against " else if (change > 0) " strengthened against " else " weakened against ",
        input$base, " from ", format(cmp$start, "%d %b %Y"), " to ", format(cmp$end, "%d %b %Y"), ".")
      tags$div(id = paste0("rate_help_", code), class = "rate-help", role = "tooltip", hidden = "hidden",
        tags$strong(paste(code, "· Reading this card")), tags$p(class = "rate-help-label", "Latest quote"), tags$p(quote),
        if (!is.null(latest)) tags$p(class = "rate-help-note", paste("Provider observation:", format(as.POSIXct(latest$timestamp, origin = "1970-01-01", tz = "UTC"), "%d %b %Y, %H:%M UTC"))),
        tags$p(class = "rate-help-label", "Strength over the period"), tags$p(movement),
        tags$p(class = "rate-help-note", "Strength compares purchasing power, not the quoted rate’s percentage change. The selected period can end before the latest quote. Rates are indicative and exclude fees."))
    })
    tagList(tags$div(class = "rate-cards", `data-count` = length(codes), cards), help)
  })

  output$comparison_chart <- echarts4r::renderEcharts4r({
    cmp <- comparison()
    validate(need(length(selected()) > 0, "Add a currency to begin your comparison."),
             need(nrow(cmp$data) > 0, if (store$values$busy) "Loading daily history…" else "No aligned observations for this period. Try another range or retry history."))
    line_chart(cmp$data, selected(), input$metric %||% "performance", input$base, dark())
  })
  three_d_requested <- reactiveVal(FALSE)
  observeEvent(input$chart_dimension, {
    if (identical(input$chart_dimension, "3d")) three_d_requested(TRUE)
  })
  output$three_d_slot <- renderUI({
    if (!three_d_requested()) return(NULL)
    # Create once, on demand. Retain the scene when hidden instead of leaving
    # Plotly's event callbacks attached to a removed WebGL element.
    conditionalPanel("input.chart_dimension == '3d'", class = "three-d-wrap", role = "group", `aria-label` = "Optional 3D currency chart; choose a measure above or use exact chart values for accessible comparison",
      plotly::plotlyOutput("strength_3d", height = "405px"))
  })
  output$strength_3d <- plotly::renderPlotly({
    req(identical(input$chart_dimension, "3d"))
    cmp <- comparison()
    validate(need(nrow(cmp$data) > 0, "Choose currencies with history to explore the 3D view."))
    strength_chart_3d(cmp$data, selected(), input$base, dark(), metric = input$metric %||% "performance")
  })
  output$change_chart <- echarts4r::renderEcharts4r({
    summary <- comparison()$summary
    validate(need(nrow(summary) > 0, "Choose currencies with history to compare period strength."))
    change_chart(summary, dark(), c(input$base, selected()))
  })
  output$chart_legend <- renderUI({
    tags$div(class = "chart-legend", lapply(selected(), function(code) tags$button(type = "button", class = "legend-button",
      `data-legend-currency` = code, `aria-pressed` = "true", `aria-label` = paste("Toggle", code, "chart line and focus its location"),
      tags$span(class = "legend-line", style = paste0("--currency-color:", color_for(code))), code)))
  })
  output$chart_caption <- renderUI({
    cmp <- comparison()
    if (!nrow(cmp$data)) return(tags$p(class = "panel-caption", "History is retrieved once and reused across your comparisons."))
    note <- switch(input$metric %||% "performance",
      performance = "Currency strength since the start. Positive means the selected currency strengthened against your base.",
      rate = paste0("Units of each currency per 1 ", input$base, ". Different units share this axis; use Change % to compare strength."))
    if (identical(input$chart_dimension, "3d")) note <- paste(note, "Drag to rotate; 2D is easier to read precisely.")
    latest <- store$values$latest
    provisional <- !is.null(latest) && as.Date(latest$date) %in% cmp$data$date
    missing <- if (length(cmp$missing)) paste("No observations:", paste(cmp$missing, collapse = ", "), ".") else ""
    tags$p(class = "panel-caption", note, tags$br(), "Daily observations · ", format(cmp$start, "%d %b %Y"), "–", format(cmp$end, "%d %b %Y"),
      if (provisional) " · Latest day is an indicative snapshot." else ".", " ", missing)
  })
  output$chart_values <- renderTable({
    data <- comparison()$data
    if (!nrow(data)) return(NULL)
    data.frame(Date = format(data$date, "%Y-%m-%d"), Currency = data$currency,
               `Rate per base` = vapply(data$rate, format_rate, character(1)),
               `Strength %` = sprintf("%+.4f", data$performance), check.names = FALSE)
  }, striped = FALSE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$observation_input <- renderUI({
    dates <- sort(unique(comparison()$data$date), decreasing = TRUE)
    if (!length(dates)) return(tags$span("No observations"))
    tags$select(id = "observation", name = "observation", class = "shiny-input-select form-select", `aria-labelledby` = "observation_label",
      lapply(seq_along(dates), function(i) tags$option(value = as.character(dates[i]), selected = if (dates[i] == isolate(observation())) "selected" else NULL,
                                                     format(dates[i], "%d %b %Y"))))
  })
  observeEvent(input$chart_date, {
    date <- as.character(input$chart_date)
    if (date %in% as.character(unique(comparison()$data$date))) updateSelectInput(session, "observation", selected = date)
  })

  output$world_map <- leaflet::renderLeaflet({
    shape <- isolate(map_values())
    leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 0, maxZoom = 7, zoomSnap = 0.25, worldCopyJump = FALSE,
      zoomControl = TRUE, preferCanvas = TRUE, attributionControl = TRUE, maxBounds = list(list(-83, -180), list(85, 180)))) |>
      leaflet::fitBounds(lng1 = -175, lat1 = -57, lng2 = 180, lat2 = 79) |>
      leaflet::addPolygons(data = shape, layerId = ~iso3, color = "#F7F7F2", weight = 0.8,
        fillColor = ~fill, fillOpacity = 1, label = ~label, group = "world",
        highlightOptions = leaflet::highlightOptions(weight = 2, color = "#748879", bringToFront = TRUE)) |>
      leaflet::addControl(html = '<span>Natural Earth · Current currency geography</span>', position = "bottomleft")
  })

  map_hidden_codes <- reactiveVal(character())
  observeEvent(input$toggle_map_currency, {
    code <- input$toggle_map_currency$code
    if (length(code) != 1 || !code %in% c(input$base, selected())) return()
    hidden <- map_hidden_codes()
    map_hidden_codes(if (code %in% hidden) setdiff(hidden, code) else c(hidden, code))
  })
  map_values <- reactive({
    cmp <- comparison()
    date <- observation()
    series <- cmp$data[cmp$data$date == date, ]
    codes <- setdiff(unique(c(input$base, selected())), map_hidden_codes())
    focus <- focused()
    shape <- world
    shape$currency <- vapply(shape$iso3, function(iso) {
      used <- unique(countries$currency[countries$iso3 == iso])
      if (!is.null(focus) && focus %in% intersect(used, codes)) return(focus)
      hit <- intersect(codes, used)
      if (length(hit)) hit[1] else if (length(used)) used[1] else ""
    }, character(1))
    shape$selected <- shape$currency %in% codes
    shape$change <- series$performance[match(shape$currency, series$currency)]
    shape$rate <- series$rate[match(shape$currency, series$currency)]
    shape$rate[shape$currency == input$base] <- 1
    shape$change[shape$currency == input$base] <- 0
    neutral <- if (dark()) "#304660" else "#BAC9BE"
    shape$fill <- vapply(seq_len(nrow(shape)), function(i) {
      if (!shape$selected[i]) return(neutral)
      if (identical(input$map_mode, "change")) {
        performance_color(shape$change[i], dark())
      } else color_for(shape$currency[i])
    }, character(1))
    shape$label <- paste(shape$name, ifelse(nzchar(shape$currency), paste0(" · ", shape$currency), ""))
    shape
  })
  observe({
    shape <- map_values()
    proxy <- leaflet::leafletProxy("world_map", session)
    session$sendCustomMessage("exchange-map-style", list(ids = as.list(shape$iso3), fills = as.list(shape$fill),
      border = if (dark()) "#13243A" else "#ECE8DE", focused = as.list(shape$currency == (focused() %||% "")),
      countries = as.list(names(map_locations())), countryBorder = if (dark()) "#F4C175" else "#935D16"))
    proxy <- leaflet::clearGroup(proxy, "islands")
    # Tiny countries have a large enough marker to remain discoverable.
    tiny <- countries[!countries$iso3 %in% shape$iso3 & countries$currency %in% setdiff(c(input$base, selected()), map_hidden_codes()), ]
    tiny <- tiny[!duplicated(tiny$iso3) & is.finite(tiny$lat) & is.finite(tiny$lng), ]
    if (nrow(tiny)) leaflet::addCircleMarkers(proxy, data = tiny, lng = ~lng, lat = ~lat, layerId = ~iso3,
      group = "islands", radius = 6, weight = 1, color = "#FFFFFF", fillOpacity = 1,
      fillColor = unname(vapply(tiny$currency, map_color_for, character(1))), label = ~paste(name, currency, sep = " · "))
  })
  output$map_legend <- renderUI({
    tags$div(class = "map-legend-wrapper",
      tags$div(class = "map-legend", `aria-label` = "Show or hide currency highlights on the map",
        lapply(unique(c(input$base, selected())), function(code) {
          visible <- !code %in% map_hidden_codes()
          tags$button(type = "button", class = paste("map-legend-button", if (!visible) "is-muted" else ""),
            `data-map-currency` = code, `aria-pressed` = if (visible) "true" else "false",
            `aria-label` = paste("Show", code, "on the map"),
            tags$i(style = paste0("--currency-color:", map_color_for(code)), `aria-hidden` = "true"),
            paste0(code, if (code == input$base) " (base)" else ""))
        })),
      if (identical(input$map_mode, "change")) tags$div(class = "map-color-key",
        tags$span(tags$i(style = paste0("--currency-color:", performance_color(-1, dark())), `aria-hidden` = "true"), "Weaker"),
        tags$span(tags$i(style = paste0("--currency-color:", performance_color(0, dark())), `aria-hidden` = "true"), "Unchanged"),
        tags$span(tags$i(style = paste0("--currency-color:", performance_color(1, dark())), `aria-hidden` = "true"), "Stronger"),
        tags$span(tags$i(style = paste0("--currency-color:", performance_color(NA_real_, dark())), `aria-hidden` = "true"), "No data"),
        tags$span(tags$i(style = paste0("--currency-color:", if (dark()) "#304660" else "#BAC9BE"), `aria-hidden` = "true"), "Not shown")) else NULL)
  })
  country_choices <- setNames(unique(countries$iso3), countries$name[match(unique(countries$iso3), countries$iso3)])
  updateSelectizeInput(session, "country_search", choices = c("Choose a country…" = "", country_choices), selected = "")
  add_to_comparison <- function(code) {
    if (!length(code) || !code %in% available()) return(FALSE)
    map_hidden_codes(setdiff(map_hidden_codes(), code))
    focused(code)
    if (code %in% c(input$base, selected())) return(TRUE)
    if (length(selected()) >= 6) {
      session$sendCustomMessage("exchange-toast", "Country selected. Remove one currency to add another; the comparison limit is six.")
      return(FALSE)
    }
    updateSelectizeInput(session, "currencies", selected = unique(c(selected(), code)))
    session$sendCustomMessage("exchange-toast", paste(code, "added to your comparison."))
    TRUE
  }
  clear_map_locations <- function(iso = NULL) {
    locations <- map_locations()
    targets <- if (is.null(iso)) names(locations) else intersect(iso, names(locations))
    if (!length(targets)) return()
    removed_codes <- unique(unlist(locations[targets], use.names = FALSE))
    remaining <- locations[setdiff(names(locations), targets)]
    # Shared currencies stay until their last map-selected location is cleared.
    unused <- setdiff(removed_codes, unlist(remaining, use.names = FALSE))
    remove <- setdiff(intersect(unused, map_added_currencies()), input$base)
    map_locations(remaining)
    map_added_currencies(setdiff(map_added_currencies(), unused))
    if (length(remove)) updateSelectizeInput(session, "currencies", selected = setdiff(selected(), remove))
    if (!is.null(selected_country()) && selected_country() %in% targets) selected_country(NULL)
    if (!is.null(focused()) && focused() %in% unused) focused(NULL)
    updateSelectizeInput(session, "country_search", selected = "")
    session$sendCustomMessage("exchange-toast", if (is.null(iso)) "Map selections cleared. Manually chosen currencies were kept." else "Country deselected.")
  }
  choose_country <- function(iso, toggle = TRUE) {
    if (length(iso) != 1 || !iso %in% countries$iso3) return()
    supported_codes <- intersect(unique(countries$currency[countries$iso3 == iso]), available())
    selected_codes <- intersect(supported_codes, selected())
    if (toggle && length(selected_codes)) {
      locations <- map_locations()
      keep <- !vapply(locations, function(x) any(x %in% selected_codes), logical(1))
      map_locations(locations[keep])
      map_added_currencies(setdiff(map_added_currencies(), selected_codes))
      updateSelectizeInput(session, "currencies", selected = setdiff(selected(), selected_codes))
      selected_country(NULL)
      focused(NULL)
      updateSelectizeInput(session, "country_search", selected = "")
      session$sendCustomMessage("exchange-toast", paste(paste(selected_codes, collapse = ", "), "removed from your comparison."))
      return()
    }
    if (toggle && iso %in% names(map_locations())) {
      clear_map_locations(iso)
      return()
    }
    selected_country(iso)
    codes <- unique(countries$currency[countries$iso3 == iso])
    supported <- intersect(codes, available())
    # Prefer an already included currency, otherwise add one supported local
    # currency. Territories with several currencies retain explicit extra buttons.
    included <- intersect(supported, c(input$base, selected()))
    code <- if (length(supported)) if (length(included)) included[1] else supported[1] else character()
    locations <- map_locations()
    locations[iso] <- list(code)
    map_locations(locations)
    if (length(code)) {
      added <- !code %in% c(input$base, selected())
      if (add_to_comparison(code) && added) map_added_currencies(unique(c(map_added_currencies(), code)))
    } else session$sendCustomMessage("exchange-toast", "Country selected, but its currency is unavailable from this provider.")
  }
  observeEvent(input$world_map_shape_click, choose_country(input$world_map_shape_click$id))
  observeEvent(input$world_map_marker_click, choose_country(input$world_map_marker_click$id))
  observeEvent(input$country_search, {
    if (!nzchar(input$country_search)) return()
    choose_country(input$country_search, toggle = FALSE)
    place <- countries[countries$iso3 == input$country_search, ][1, ]
    if (nrow(place) && is.finite(place$lat)) leaflet::setView(leaflet::leafletProxy("world_map", session), place$lng, place$lat, zoom = 4)
  })
  observeEvent(input$clear_map, clear_map_locations())
  observeEvent(input$currencies, {
    # A manual removal ends ownership by the map, even if the user later re-adds it.
    lost <- setdiff(map_added_currencies(), c(input$base, input$currencies))
    if (!length(lost)) return()
    map_added_currencies(setdiff(map_added_currencies(), lost))
    locations <- map_locations()
    removed <- names(locations)[vapply(locations, function(codes) any(codes %in% lost), logical(1))]
    map_locations(locations[setdiff(names(locations), removed)])
    if (!is.null(selected_country()) && selected_country() %in% removed) selected_country(NULL)
    if (!is.null(focused()) && focused() %in% lost) focused(NULL)
  }, ignoreInit = TRUE)
  output$country_detail <- renderUI({
    iso <- selected_country()
    if (is.null(iso)) return(tags$div(class = "country-detail", tags$strong("Every currency has a place."),
      tags$p("Click a country to add its currency; click it again to deselect. Clear resets map selections, keeping manually chosen currencies. Shared currencies highlight all places that use them.")))
    places <- countries[countries$iso3 == iso, ]
    if (!nrow(places)) return(tags$div(class = "country-detail", "Currency mapping is unavailable for this territory."))
    date <- observation()
    rows <- if (is.na(date)) history()[FALSE, ] else history()[history()$date == date, ]
    rates <- setNames(rows$rate, rows$currency)
    tags$div(class = "country-detail", tags$strong(places$name[1]),
      lapply(seq_len(nrow(places)), function(i) {
        code <- places$currency[i]
        rate <- cross_rate(rates, input$base, code)
        tags$div(tags$p(paste0(code, " · ", if (code %in% available()) currency_name(code) else places$currency_name[i])),
          tags$p(paste0("1 ", input$base, " = ", format_rate(rate), if (is.finite(rate)) paste0(" ", code) else "", " · ", if (!is.na(date)) format(date, "%d %b %Y") else "No observation")),
          tags$div(class = "country-actions", if (code %in% available() && !code %in% c(input$base, selected()))
            tags$button(type = "button", class = "quiet-button", `data-add-currency` = code, paste("Add", code, "to comparison"))
          else if (code %in% c(input$base, selected())) tags$span("Included in this view")
          else tags$span("This currency is not supplied by the current provider.")))
      }), tags$p(class = "field-help", "Current currency use, even when viewing earlier rates. Borders are simplified for exploration."))
  })
  observeEvent(input$add_currency, {
    code <- input$add_currency$code
    add_to_comparison(code)
  })
  observeEvent(input$focus_currency, focused(input$focus_currency$code))
  observeEvent(input$fit_map, {
    focus <- focused()
    codes <- if (!is.null(focus) && focus %in% c(input$base, selected())) focus else c(input$base, selected())
    shape <- world[world$iso3 %in% countries$iso3[countries$currency %in% codes], ]
    if (!nrow(shape)) return()
    bounds <- sf::st_bbox(shape)
    if (bounds["xmax"] - bounds["xmin"] > 300) leaflet::setView(leaflet::leafletProxy("world_map", session), 14, 24, zoom = 1.4)
    else leaflet::fitBounds(leaflet::leafletProxy("world_map", session), bounds["xmin"], max(-65, bounds["ymin"]), bounds["xmax"], min(80, bounds["ymax"]))
  })

  output$conversion_result <- renderUI({
    latest <- store$values$latest
    amount <- suppressWarnings(as.numeric(input$amount))
    if (length(amount) != 1 || !is.finite(amount) || amount < 0 || amount > 1e12)
      return(tags$div(class = "conversion-output", role = "status", "Enter an amount between 0 and 1 trillion."))
    if (is.null(latest)) return(tags$div(class = "conversion-output", "Waiting for current rates…"))
    rate <- cross_rate(latest$rates, input$from, input$to)
    tags$div(class = "conversion-output", `aria-live` = "polite",
      tags$div(class = "conversion-equation", paste(formatC(amount, format = "f", digits = 2, big.mark = ","), input$from, "equals")),
      tags$div(class = "conversion-value", if (is.finite(rate)) formatC(amount * rate, format = "f", digits = 2, big.mark = ",") else "Rate unavailable",
        tags$small(input$to)))
  })
  output$conversion_note <- renderUI({
    latest <- store$values$latest
    if (is.null(latest)) return(NULL)
    rate <- cross_rate(latest$rates, input$from, input$to)
    tags$p(class = "panel-caption", paste("1", input$from, "=", format_rate(rate), input$to), tags$br(),
      "Observed ", format(as.POSIXct(latest$timestamp, origin = "1970-01-01", tz = "UTC"), "%d %b %Y, %H:%M UTC"), " · Indicative, before fees.")
  })
  observeEvent(input$swap, {
    from <- input$from; to <- input$to
    updateSelectizeInput(session, "from", selected = to)
    updateSelectizeInput(session, "to", selected = from)
  })
  observeEvent(input$quick_amount, {
    amount <- suppressWarnings(as.numeric(input$quick_amount$value))
    if (length(amount) == 1 && amount %in% c(100, 1000, 10000)) updateNumericInput(session, "amount", value = amount)
  })
  output$quote_context <- renderUI({
    latest <- store$values$latest
    if (is.null(latest)) return(tags$p(class = "panel-caption", "Waiting for the latest observation."))
    rate <- cross_rate(latest$rates, input$from, input$to)
    tags$div(class = "quote-details", tags$dl(
      tags$dt("Reference quote"), tags$dd(paste("1", input$from, "=", format_rate(rate), input$to)),
      tags$dt("Inverse quote"), tags$dd(paste("1", input$to, "=", format_rate(if (is.finite(rate)) 1 / rate else NA_real_), input$from)),
      tags$dt("Observed (UTC)"), tags$dd(format(as.POSIXct(latest$timestamp, origin = "1970-01-01", tz = "UTC"), "%d %b %Y · %H:%M")),
      tags$dt("Provider"), tags$dd("APILayer"),
      tags$dt("Quote type"), tags$dd("Indicative snapshot")))
  })
  output$conversion_basket <- renderUI({
    latest <- store$values$latest
    amount <- suppressWarnings(as.numeric(input$amount))
    if (is.null(latest) || length(amount) != 1 || !is.finite(amount) || amount < 0 || amount > 1e12) return(NULL)
    codes <- unique(c(input$to, selected()))
    tags$div(class = "basket-grid", lapply(codes, function(code) {
      rate <- cross_rate(latest$rates, input$from, code)
      tags$div(class = "basket-item", tags$strong(paste(code, "·", currency_name(code))),
        tags$span(if (is.finite(rate)) formatC(amount * rate, format = "f", digits = 2, big.mark = ",") else "Unavailable"),
        tags$small(paste("1", input$from, "=", format_rate(rate), code)))
    }))
  })

  table_data <- reactive({
    mode <- input$data_mode %||% "summary"
    if (mode == "quotes") return(current_quotes(store$values$latest, input$base, currency_names(), countries))
    if (mode == "history") return(detailed_history(comparison()$data, input$base, currency_names(), store$values$latest))
    detailed_summary(period_summary(), comparison()$data, currency_names(), observation(), store$values$latest, input$base)
  })
  output$summary_table <- DT::renderDT({
    data <- table_data()
    validate(need(nrow(data) > 0, "No comparison data available yet."))
    number_columns <- names(data)[vapply(data, is.numeric, logical(1))]
    widget <- DT::datatable(data, rownames = FALSE, selection = "single", class = "compact", escape = TRUE,
      callback = DT::JS("var scroller=table.table().container().querySelector('.dataTables_scrollBody');if(scroller){scroller.tabIndex=0;scroller.setAttribute('role','region');scroller.setAttribute('aria-label','Currency data; scroll horizontally for more columns');scroller.setAttribute('aria-describedby','table_scroll_help');if(window.ExchangeTableScroll)window.ExchangeTableScroll(scroller);}function focusRows(){table.rows({page:'current'}).nodes().to$().attr('tabindex','0');if(scroller&&scroller._exchangeScrollUpdate)scroller._exchangeScrollUpdate();}focusRows();table.on('draw',focusRows);table.on('keydown','tbody tr',function(event){if(event.key==='Enter'||event.key===' '){event.preventDefault();this.click();}});"),
      options = list(dom = "lftip", paging = TRUE, pageLength = 15, lengthMenu = c(15, 30, 60, 100),
        scrollX = TRUE, scrollY = "460px", scrollCollapse = TRUE, ordering = TRUE, autoWidth = TRUE,
        language = list(search = "Search data:", lengthMenu = "Show _MENU_ rows", info = "_START_–_END_ of _TOTAL_ records"),
        columnDefs = list(list(className = "dt-right", targets = which(vapply(data, is.numeric, logical(1))) - 1))))
    integer_columns <- intersect(c("Observations", "Mapped places"), number_columns)
    decimal_columns <- setdiff(number_columns, integer_columns)
    if (length(decimal_columns)) widget <- DT::formatRound(widget, decimal_columns, digits = 6)
    if (length(integer_columns)) widget <- DT::formatRound(widget, integer_columns, digits = 0)
    percent_columns <- intersect(c("Strength %", "Daily strength %"), names(data))
    if (length(percent_columns)) widget <- DT::formatRound(widget, percent_columns, digits = 2)
    widget
  }, server = FALSE)
  observeEvent(input$summary_table_rows_selected, {
    row <- input$summary_table_rows_selected
    if (length(row) && row[1] <= nrow(table_data())) focused(table_data()$Currency[row[1]])
  })
  output$table_caption <- renderUI({
    mode <- input$data_mode %||% "summary"
    if (mode == "quotes") return(tags$p(class = "panel-caption", "Latest indicative quotes · Units per 1 ", input$base,
      " · Includes non-geographic instruments. This list is independent of the selected comparison and time period."))
    if (mode == "history") return(tags$p(class = "panel-caption", "Daily observed rates · Units per 1 ", input$base,
      " · Daily strength uses consecutive observed days only; gaps remain unavailable. Export contains every record, not just this page."))
    cmp <- comparison()
    if (!nrow(cmp$summary)) return(NULL)
    tags$p(class = "panel-caption", "Rates at ", format(observation(), "%d %b %Y"), " · Units per 1 ", input$base,
      ". Strength is measured from ", format(cmp$start, "%d %b %Y"), ". Low/high and their dates cover the whole selected period, not just the observation date. Select a row to focus its geography.")
  })
  output$data_stats <- renderUI({
    data <- table_data()
    if (!nrow(data)) return(NULL)
    tags$div(class = "data-stats", tags$span(tags$strong(nrow(data)), "records"),
      tags$span(tags$strong(length(unique(data$Currency))), "currencies"),
      tags$span("Full precision in CSV"))
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0("exchange-universe-", input$data_mode %||% "summary", "-", input$base, "-", Sys.Date(), ".csv"),
    content = function(file) {
      data <- table_data()
      validate(need(nrow(data) > 0, "No data to export."))
      data$Base <- input$base; data$Source <- "APILayer Exchange Rates Data"
      utils::write.csv(data, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    })

  current_state <- reactive({
    list(base = input$base, currencies = selected(), period = input$period, dates = as.character(range()),
         metric = input$metric, from = input$from, to = input$to, view = input$current_view %||% "explore")
  })
  apply_state <- function(state) {
    if (!is.list(state)) return()
    if (length(state$view) == 1 && state$view %in% c("explore", "convert", "data")) session$sendCustomMessage("exchange-view", state$view)
    for (id in c("base", "from", "to")) if (length(state[[id]]) == 1 && state[[id]] %in% available()) updateSelectizeInput(session, id, selected = state[[id]])
    if (!is.null(state$currencies)) updateSelectizeInput(session, "currencies", selected = head(intersect(unlist(state$currencies), available()), 6))
    if (length(state$period) == 1 && state$period %in% c("30", "90", "180", "365", "custom")) updateSelectInput(session, "period", selected = state$period)
    # Older shared links and saved comparisons used the redundant index scale.
    if (identical(state$metric, "index")) state$metric <- "performance"
    if (length(state$metric) == 1 && state$metric %in% c("performance", "rate")) updateRadioButtons(session, "metric", selected = state$metric)
    if (!is.null(state$dates) && length(state$dates) == 2) {
      dates <- suppressWarnings(as.Date(unlist(state$dates)))
      if (!anyNA(dates) && dates[1] >= as.Date("1999-01-01") && dates[1] <= dates[2] && dates[2] <= Sys.Date() && as.integer(dates[2] - dates[1]) <= 365)
        session$sendCustomMessage("exchange-dates", as.list(as.character(dates)))
    }
  }
  observeEvent(input$restore_state, { apply_state(input$restore_state); restored(TRUE) })
  observe({
    req(restored())
    session$sendCustomMessage("exchange-state", current_state())
  })
  observeEvent(input$share, session$sendCustomMessage("exchange-share", current_state()))
  observeEvent(input$share_fallback, showModal(modalDialog(title = "Copy this comparison link", textInput("share_url", "Comparison URL", value = input$share_fallback), easyClose = TRUE, footer = modalButton("Close"))))
  observeEvent(input$save_view, showModal(modalDialog(title = "Save comparison", textInput("view_name", "Name", value = paste(input$base, "vs", paste(selected(), collapse = ", "))),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_save", "Save view", class = "btn-primary")), easyClose = TRUE)))
  observeEvent(input$confirm_save, {
    name <- substr(trimws(input$view_name %||% ""), 1, 80)
    if (!nzchar(name)) { showNotification("Enter a name for this comparison.", type = "warning"); return() }
    session$sendCustomMessage("exchange-save", list(id = paste0(as.integer(Sys.time()), "-", sample(10000, 1)), name = name, state = current_state()))
    removeModal()
  })
  observeEvent(input$saved_views, {
    views <- tryCatch(jsonlite::fromJSON(input$saved_views, simplifyVector = FALSE), error = function(e) list())
    if (!is.list(views)) views <- list()
    views <- head(Filter(function(view) is.list(view) && is.character(view$id) && length(view$id) == 1 && is.character(view$name) && length(view$name) == 1 && is.list(view$state), views), 20)
    saved_views(views)
    choices <- c("Choose a saved view…" = "")
    if (length(views)) for (view in views) choices[view$name] <- view$id
    updateSelectInput(session, "saved_view", choices = choices, selected = "")
  })
  observeEvent(input$saved_view, {
    if (!nzchar(input$saved_view)) return()
    for (view in saved_views()) if (identical(view$id, input$saved_view)) apply_state(view$state)
  })
  observeEvent(input$delete_view, {
    if (nzchar(input$saved_view %||% "")) session$sendCustomMessage("exchange-delete", input$saved_view)
    else session$sendCustomMessage("exchange-toast", "Choose a saved view to remove it.")
  })

  output$source_footer <- renderUI({
    latest <- store$values$latest
    if (is.null(latest)) return(tags$span("APILayer · Awaiting first observation"))
    seconds <- effective_refresh(config, store$values$quota)
    cadence <- if (seconds >= 86400) paste0(round(seconds / 86400), "-day checks") else "Hourly checks"
    tags$span("APILayer · ", format(as.POSIXct(latest$timestamp, origin = "1970-01-01", tz = "UTC"), "%d %b %Y, %H:%M UTC"),
      " · ", cadence, " · ", length(latest$rates), " symbols")
  })
  observeEvent(input$about, {
    quota <- store$values$quota; latest <- store$values$latest
    showModal(modalDialog(title = "About Exchange Universe", size = "l", easyClose = TRUE, footer = modalButton("Back to the atlas"),
      tags$p("Compare rates, convert amounts and connect currencies with their geography."),
      tags$div(class = "source-grid", tags$div(class = "source-stat", tags$strong(length(available())), "Provider symbols, including non-geographic instruments"),
        tags$div(class = "source-stat", tags$strong(if (is.finite(quota$remaining %||% NA)) quota$remaining else "—"), "Requests remaining this month")),
      tags$h3("Data & freshness"),
      tags$p("Rates come from APILayer’s Exchange Rates Data API. Daily history and current observations are cached and reused across visitors. The newest chart point may be an indicative snapshot; its date is labelled in the chart."),
      tags$p(paste0("Current automatic checks run ", if (effective_refresh(config, quota) >= 86400) "daily" else "hourly",
        " while the app is running. The cadence adapts to the available request quota. Provider update frequency depends on the subscribed plan; daily history is not an hourly archive.")),
      tags$h3("Reading the charts"),
      tags$p("Exchange rate means units of a selected currency per 1 unit of your base. Strength % = (start-date rate ÷ observed rate − 1) × 100. Positive strength means the selected currency strengthened. All comparisons share the same observed start and end dates; missing observations remain missing."),
      tags$h3("Reading the map"),
      tags$p("The map shows current currency use, including countries sharing a currency. Historical currency boundaries are not reconstructed. Non-geographic instruments such as metals have no country location. Tiny territories can be found through country search. Natural Earth’s simplified borders are for exploration."),
      tags$h3("Privacy & storage"),
      tags$p("Saved comparisons and theme preferences stay in this browser. API credentials are read on the server and never sent to the browser. Server caches persist locally; production hourly archives require durable storage and a scheduled collector."),
      tags$p(tags$a(href = "https://marketplace.apilayer.com/exchangerates_data-api/tabs/api_docs", target = "_blank", rel = "noopener", "Provider documentation"),
        " · ", tags$a(href = "https://www.naturalearthdata.com/", target = "_blank", rel = "noopener", "Natural Earth"),
        " · ", tags$a(href = "https://github.com/mledoze/countries", target = "_blank", rel = "noopener", "Country metadata"))))
  })
}

shinyApp(ui, server)
