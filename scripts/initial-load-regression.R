# Source-only guard for restored widget structure; no server, credentials or API calls.
ui_source <- paste(readLines("R/ui.R", warn = FALSE), collapse = "\n")
stopifnot(grepl('class = "chart-wrap", echarts4r::echarts4rOutput', ui_source, fixed = TRUE))
stopifnot(grepl('class = "map-wrap", leaflet::leafletOutput', ui_source, fixed = TRUE))
stopifnot(!grepl('comparison_chart_slot|world_map_slot|change_chart_slot', ui_source))
cat("PASS: chart and map use original direct wrappers; no deferred-slot regression\n")
