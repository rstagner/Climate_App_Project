#Load libraries

library(tidyverse)
library(httr2)
library(jsonlite)

# Load central configuration
source("R/00_config.R")

#Fetch function for NOAA GHCN daily data using .csv.gz endpoints
fetch_noaa_raw <- function(station_id, city_name) {
  message(sprintf("Fetching raw NOAA data for %s (Station: %s)...", city_name, station_id))
  
  url <- sprintf("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/%s.csv.gz", station_id)
  
  tryCatch({
    # FIX: col_types = cols(.default = "c") forces all columns to read as characters,
    # preventing type mismatch errors when bind_rows() combines different stations.
    read_csv(url, col_names = FALSE, col_types = cols(.default = "c"), show_col_types = FALSE) |>
      mutate(CITY = city_name, STATION = station_id)
  }, error = function(e) {
    message(sprintf("  -> Failed for %s (%s): %s", city_name, station_id, e$message))
    NULL
  })
}

# Execute batch fetch across all 48 station IDs
noaa_raw_list <- imap(
  setNames(city_coords$station_info_id, city_coords$CITY),
  ~ fetch_noaa_raw(.x, .y)
)

# Save rds and check file size
saveRDS(noaa_raw_list, "data_raw/noaa_raw_snapshot.rds")

raw_noaa_path <- "data_raw/noaa_raw_snapshot.rds"
if (file.exists(raw_noaa_path)) {
  size_mb <- file.info(raw_noaa_path)$size / (1024 * 1024)
  message(sprintf("\n[Success] Full raw NOAA data exported to '%s'", csv_path))
  message(sprintf("[File Info] NOAA Raw Snapshot RDS Size: %.2f MB | Total Rows: %s", 
                  size_mb, format(nrow(noaa_raw_full), big.mark = ",")))
}
file.rename("data_raw/noaa_raw_snapshot.rds", "data_raw/noaa_raw_11-03-2026.rds")
