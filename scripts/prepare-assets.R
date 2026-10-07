args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[1] else getwd()
dir.create(file.path(root, "data"), recursive = TRUE, showWarnings = FALSE)
source(file.path(root, "R", "provider.R"))
dest <- file.path(root, "data", "world-source.geojson")
if (!file.exists(dest)) download.file("https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_50m_admin_0_countries.geojson", dest, mode = "wb", quiet = TRUE)
world <- sf::st_read(dest, quiet = TRUE)
sf::sf_use_s2(FALSE)
world <- sf::st_simplify(world, dTolerance = 0.025, preserveTopology = TRUE)
world$iso3 <- ifelse(world$ISO_A3 == "-99", world$ADM0_A3, world$ISO_A3)
world <- world[world$iso3 != "ATA", c("iso3", "NAME_EN", "geometry")]
names(world)[2] <- "name"
world <- sf::st_transform(world, 4326)
atomic_rds(world, file.path(root, "data", "world.rds"))
response <- httr::GET("https://raw.githubusercontent.com/mledoze/countries/master/countries.json", httr::timeout(40))
httr::stop_for_status(response)
raw <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
rows <- lapply(raw, function(c) {
  codes <- names(c$currencies)
  if (is.null(codes) && length(c$currencies)) codes <- unlist(c$currencies)
  if (!length(codes)) codes <- ""
  data.frame(iso3 = c$cca3, iso2 = c$cca2, name = c$name$common, currency = codes,
    currency_name = vapply(codes, function(code) {
      item <- c$currencies[[code]]
      if (is.list(item)) item$name %||% "" else ""
    }, character(1)),
    lat = as.numeric(c$latlng[[1]] %||% NA), lng = as.numeric(c$latlng[[2]] %||% NA),
    region = c$region, stringsAsFactors = FALSE)
})
countries <- do.call(rbind, rows)
# Current-use corrections with dated primary references recorded in data/SOURCES.md.
countries$currency[countries$iso3 == "BGR"] <- "EUR"
countries$currency_name[countries$iso3 == "BGR"] <- "Euro"
countries$currency[countries$iso3 %in% c("CUW", "SXM")] <- "XCG"
countries$currency_name[countries$iso3 %in% c("CUW", "SXM")] <- "Caribbean guilder"
countries$currency[countries$iso3 == "ZWE" & countries$currency == "ZWL"] <- "ZWG"
countries$currency_name[countries$iso3 == "ZWE" & countries$currency == "ZWG"] <- "Zimbabwe Gold"
utils::write.csv(countries, file.path(root, "data", "countries.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("Prepared", nrow(world), "polygons and", length(unique(countries$iso3)), "country/territory records.\n")
