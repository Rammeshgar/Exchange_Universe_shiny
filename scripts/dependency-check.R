if (.Platform$OS.type == "windows") {
  candidate <- file.path(Sys.getenv("USERPROFILE"), "AppData", "Local", "R", "win-library", paste(R.version$major, strsplit(R.version$minor, "\\.")[[1]][1], sep = "."))
  if (dir.exists(candidate)) .libPaths(c(candidate, .libPaths()))
}
packages <- c("shiny", "bslib", "leaflet", "sf", "echarts4r", "DT", "httr", "jsonlite", "htmlwidgets", "callr", "plotly", "rsconnect")
versions <- vapply(packages, function(package) if (requireNamespace(package, quietly = TRUE)) as.character(utils::packageVersion(package)) else "MISSING", character(1))
print(data.frame(package = packages, version = unname(versions)), row.names = FALSE)
