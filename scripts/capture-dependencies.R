# Record the installed, reviewed dependency graph without installing or updating it.
source("scripts/dependency-check.R")
direct <- packages[packages != "rsconnect"]
queue <- direct; seen <- character(); records <- list()
while (length(queue)) {
  package <- queue[1]; queue <- queue[-1]
  if (package %in% c(seen, "R")) next
  seen <- c(seen, package)
  if (!requireNamespace(package, quietly = TRUE)) stop(paste("Dependency not available:", package))
  # Read the installed package's own metadata; do not rely on library enumeration
  # when the host resolves packages from its configured namespace paths.
  folder <- getNamespaceInfo(asNamespace(package), "path")
  description <- as.list(read.dcf(file.path(folder, "DESCRIPTION"))[1, ])
  fields <- unlist(description[c("Depends", "Imports", "LinkingTo")], use.names = FALSE)
  fields <- fields[!is.na(fields)]
  if (length(fields)) {
    dependencies <- trimws(gsub("\\s*\\([^)]*\\)", "", unlist(strsplit(fields, ","))))
    queue <- unique(c(queue, dependencies[nzchar(dependencies)]))
  }
  if (!is.null(description$Priority) && description$Priority %in% c("base", "recommended")) next
  records[[package]] <- list(Package = package, Version = description$Version,
                            Source = "Repository", Repository = "CRAN")
}
records <- records[sort(names(records))]
stopifnot(length(records) >= length(direct))
lock <- list(R = list(Version = as.character(getRversion()),
  Repositories = list(list(Name = "CRAN", URL = "https://cloud.r-project.org"))), Packages = records)
jsonlite::write_json(lock, "renv.lock", pretty = TRUE, auto_unbox = TRUE)
cat("Recorded", length(records), "packages in renv.lock. No dependencies were changed.\n")
