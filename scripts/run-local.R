args <- commandArgs(trailingOnly = TRUE)
if (.Platform$OS.type == "windows") {
  invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.utf8")))
}
port <- if (length(args)) as.integer(args[1]) else 4888L
if (.Platform$OS.type == "windows") {
  user_library <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", paste(R.version$major, strsplit(R.version$minor, "\\.")[[1]][1], sep = "."))
  if (dir.exists(user_library)) .libPaths(c(user_library, .libPaths()))
}
shiny::runApp(getwd(), host = "127.0.0.1", port = port, launch.browser = FALSE)
