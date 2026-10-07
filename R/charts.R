chart_palette <- function(dark = FALSE) {
  if (dark) list(text = "#ADBED5", grid = "#32445E", surface = "#101D32", ink = "#EDF4FF")
  else list(text = "#52635F", grid = "#C9CFC4", surface = "#F4F0E7", ink = "#243A38")
}

chart_widget <- function(options) {
  widget <- echarts4r::e_charts(data.frame(x = 1:2, y = 1:2), x)
  widget$x$opts <- options
  widget$x$dispose <- TRUE
  widget
}

line_chart <- function(data, currencies, metric, base, dark = FALSE) {
  palette <- chart_palette(dark)
  dates <- sort(unique(data$date))
  series <- lapply(currencies, function(code) {
    one <- data[data$currency == code, ]
    if (!nrow(one)) return(NULL)
    # Preserve gaps as nulls, instead of drawing through missing dates.
    complete <- merge(data.frame(date = seq(min(dates), max(dates), by = "day")), one, by = "date", all.x = TRUE)
    list(name = code, type = "line", smooth = FALSE, showSymbol = FALSE,
         symbol = "circle", symbolSize = 6, connectNulls = FALSE,
         lineStyle = list(width = 2.5, color = currency_color(code, dark, c(base, currencies))),
         itemStyle = list(color = currency_color(code, dark, c(base, currencies))),
         emphasis = list(focus = "series", lineStyle = list(width = 3.5)),
         data = lapply(seq_len(nrow(complete)), function(i) list(as.character(complete$date[i]), complete[[metric]][i])))
  })
  series <- Filter(Negate(is.null), series)
  unit <- switch(metric, performance = "Currency strength (%)", index = "Starting strength = 100", rate = paste("Units per 1", base))
  formatter <- switch(metric,
    performance = htmlwidgets::JS("function(v){return v.toFixed(Math.abs(v)<1?2:1)+'%';}"),
    index = htmlwidgets::JS("function(v){return v.toFixed(1);}"),
    rate = htmlwidgets::JS("function(v){return Math.abs(v)>=1000?(v/1000).toFixed(1)+'k':v.toLocaleString('en-US',{maximumFractionDigits:4});}"))
  widget <- chart_widget(list(animation = FALSE, backgroundColor = "transparent",
    textStyle = list(fontFamily = "Atlas, system-ui, sans-serif", color = palette$text),
    aria = list(enabled = TRUE, description = paste("Daily", unit, "for", paste(currencies, collapse = ", "), "against", base, ". Chart values are available in the adjacent table.")),
    grid = list(left = 59, right = 23, top = 32, bottom = 55, containLabel = FALSE),
    tooltip = list(trigger = "axis", confine = TRUE, backgroundColor = palette$surface,
      borderColor = palette$grid, textStyle = list(color = palette$ink, fontSize = 12),
      valueFormatter = htmlwidgets::JS(paste0("function(v){return v===null?'Unavailable':Number(v).toLocaleString('en-US',{maximumFractionDigits:", if (metric == "rate") 6 else 2, "})", if (metric == "performance") "+'%'" else "", ";}"))),
    legend = list(show = FALSE, data = as.list(currencies)),
    xAxis = list(type = "time", boundaryGap = FALSE, axisLine = list(show = FALSE),
                 axisTick = list(show = FALSE), splitLine = list(show = FALSE),
                 axisLabel = list(color = palette$text, fontSize = 10, hideOverlap = TRUE,
                                  formatter = "{dd} {MMM}")),
    yAxis = list(type = "value", scale = TRUE, name = unit, nameGap = 16,
      nameTextStyle = list(color = palette$text, fontSize = 10, align = "left"),
      axisLine = list(show = FALSE), axisTick = list(show = FALSE),
      axisLabel = list(color = palette$text, fontSize = 10, formatter = formatter),
      splitLine = list(lineStyle = list(color = palette$grid, type = "dashed"))),
    dataZoom = list(list(type = "inside", filterMode = "none", zoomOnMouseWheel = FALSE),
                    list(type = "slider", height = 14, bottom = 5, borderColor = "transparent",
                         backgroundColor = palette$grid, fillerColor = if (dark) "#70d9df22" else "#087a8718",
                         showDetail = FALSE, showDataShadow = FALSE, handleSize = "100%")),
    series = series))
  htmlwidgets::onRender(widget, "function(el){
    var chart=echarts.getInstanceByDom(el), active=null;
    if(!chart)return;
    chart.on('mouseover',function(e){if(e.componentType==='series')active=e.seriesName;});
    chart.on('globalout',function(){active=null;});
    var format=chart.getOption().tooltip[0].valueFormatter;
    function escape(s){return String(s).replace(/[&<>\"']/g,function(c){return {'&':'&amp;','<':'&lt;','>':'&gt;','\"':'&quot;',\"'\":'&#39;'}[c];});}
    chart.setOption({tooltip:{formatter:function(rows){
      rows=Array.isArray(rows)?rows.slice():[rows];
      rows.sort(function(a,b){return Number(b.seriesName===active)-Number(a.seriesName===active);});
      var date=rows[0]&&rows[0].value&&rows[0].value[0];
      return escape(date||'')+rows.map(function(p){
        var value=Array.isArray(p.value)?p.value[1]:p.value;
        var text=value==null?'Unavailable':typeof format==='function'?format(value):String(value);
        return '<br>'+p.marker+'<span style=\"font-weight:'+(p.seriesName===active?'700':'400')+'\">'+escape(p.seriesName)+' &nbsp; '+escape(text)+'</span>';
      }).join('');
    }}});
  }")
}

strength_chart_3d <- function(data, currencies, base, dark = FALSE, metric = "performance") {
  stopifnot(metric %in% c("performance", "index", "rate"))
  palette <- chart_palette(dark)
  dates <- seq(min(data$date), max(data$date), by = "day")
  ticks <- unique(round(seq(1, length(dates), length.out = min(4, length(dates)))))
  fig <- plotly::plot_ly()
  lanes <- currencies[currencies %in% unique(data$currency)]
  for (i in seq_along(lanes)) {
    code <- lanes[i]
    one <- data[data$currency == code, ]
    complete <- merge(data.frame(date = dates), one, by = "date", all.x = TRUE)
    measure <- switch(metric, performance = paste0("Strength: ", sprintf("%+.2f%%", complete$performance)),
      index = paste0("Index 100: ", sprintf("%.2f", complete$index)),
      rate = paste0("Quoted rate: ", formatC(complete$rate, digits = 6, format = "f"), " ", code))
    text <- paste0(code, " · ", complete$date, "<br>", measure,
      "<br>1 ", base, " = ", formatC(complete$rate, digits = 6, format = "f"), " ", code)
    fig <- plotly::add_trace(fig, x = as.numeric(complete$date), y = rep(i, nrow(complete)),
      z = complete[[metric]], type = "scatter3d", mode = "lines", name = code, text = text,
      hoverinfo = "text", connectgaps = FALSE, line = list(width = 5, color = currency_color(code, dark, c(base, currencies))))
  }
  axis <- list(gridcolor = palette$grid, zerolinecolor = palette$grid, color = palette$text,
    showbackground = FALSE, tickfont = list(size = 10), titlefont = list(size = 11))
  fig <- plotly::layout(fig, showlegend = FALSE, paper_bgcolor = palette$surface, plot_bgcolor = palette$surface,
    font = list(family = "Atlas, system-ui, sans-serif", color = palette$text),
    margin = list(l = 10, r = 24, b = 30, t = 8),
    scene = list(xaxis = c(axis, list(title = "Date", tickmode = "array", tickvals = as.numeric(dates[ticks]), ticktext = format(dates[ticks], "%d %b"))),
      yaxis = c(axis, list(title = "Currency", tickmode = "array", tickvals = seq_along(lanes), ticktext = lanes, range = c(.5, length(lanes) + .5))),
      zaxis = c(axis, list(title = switch(metric, performance = "Strength (%)", index = "Starting strength = 100", rate = paste("Units per 1", base)),
        ticksuffix = if (metric == "performance") "%" else "", rangemode = "normal")),
      aspectmode = "manual", aspectratio = list(x = 1.7, y = 1, z = .8),
      camera = list(eye = list(x = 1.65, y = -1.8, z = 1.1))),
    uirevision = paste(base, paste(lanes, collapse = ",")))
  fig <- plotly::config(fig, displaylogo = FALSE, scrollZoom = TRUE, responsive = TRUE,
    modeBarButtonsToRemove = c("toImage", "resetCameraLastSave3d"),
    locale = "en")
  htmlwidgets::onRender(fig, "function(el){el.on('plotly_click',function(event){var x=event.points&&event.points[0]&&event.points[0].x;if(Number.isFinite(x)&&typeof window.Shiny?.setInputValue==='function')Shiny.setInputValue('chart_date',new Date(x*86400000).toISOString().slice(0,10),{priority:'event'});});}")
}

change_chart <- function(summary, dark = FALSE, palette_codes = summary$currency) {
  palette <- chart_palette(dark)
  summary <- summary[order(summary$change), ]
  chart_widget(list(animation = FALSE, backgroundColor = "transparent",
    textStyle = list(fontFamily = "Atlas, system-ui, sans-serif"),
    aria = list(enabled = TRUE, description = "Currency strength over the selected period. Positive means stronger against the base. Exact values are in the comparison table."),
    grid = list(left = 55, right = 56, top = 8, bottom = 30),
    tooltip = list(trigger = "axis", axisPointer = list(type = "shadow"), confine = TRUE,
                   backgroundColor = palette$surface, textStyle = list(color = palette$ink),
                   valueFormatter = htmlwidgets::JS("function(v){return (v>=0?'+':'')+v.toFixed(2)+'%';}")),
    xAxis = list(type = "value", axisLabel = list(color = palette$text, fontSize = 10, formatter = "{value}%"),
      splitLine = list(lineStyle = list(color = palette$grid, type = "dashed")), axisLine = list(show = FALSE)),
    yAxis = list(type = "category", data = as.list(summary$currency), axisLabel = list(color = palette$ink, fontSize = 11),
                 axisTick = list(show = FALSE), axisLine = list(show = FALSE)),
    series = list(list(type = "bar", barMaxWidth = 16, data = lapply(seq_len(nrow(summary)), function(i)
      list(value = summary$change[i], itemStyle = list(color = currency_color(summary$currency[i], dark, palette_codes), borderRadius = 3))),
      label = list(show = TRUE, position = "right", color = palette$ink, fontSize = 10,
                   formatter = htmlwidgets::JS("function(p){return (p.value>=0?'+':'')+p.value.toFixed(2)+'%';}"))))))
}
