
#pull NOAA GHCN-Daily station reference files
url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/"

download.file(paste0(url, "ghcnd-stations.txt"),
              paste0("ghcnd-stations_", Sys.Date(), ".txt"))

download.file(paste0(url, "ghcnd-inventory.txt"),
              paste0("ghcnd-inventory_", Sys.Date(), ".txt"))

#look at the raw file
read_lines("ghcnd-stations_2026-11-16.txt", n_max = 5)
read_lines("ghcnd-inventory_2026-11-16.txt", n_max = 5)
