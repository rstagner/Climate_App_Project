library(tidyverse)

#load the 48 matched stations
city_stations <- readRDS("data/city_stations.rds")

#column names for NOAA data from documentation
noaa_cols <- c("station_id", "date", "element", "value", 
               "mflag", "qflag", "sflag", "obs_time")

#download one station, keeping 1990 onward and the three elements we need
fetch_station <- function(id) {
  url <- paste0("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/",
                id, ".csv.gz")
  message("Fetching", id)
  tryCatch(
    read_csv(url, col_names = noaa_cols, col_types = cols(.default = "c")) |> 
      filter(element %in% c("TMAX", "TMIN", "PRCP"),
             date >= "19900101"),
    error = function(e) {
      message(" failed; ", e$message)
      NULL
    }
  )
}

#download all 48
noaa_list <- map(city_stations$station_info_id, fetch_station)
names(noaa_list) <- city_stations$station_info_id

#report any failures
failed <- names(noaa_list)[map_lgl(noaa_list, is.null)]
if (length(failed) >0) message("No data for: ", paste(failed, collapse = ", "))

#combine and save with the download date
noaa_raw <- list_rbind(noaa_list)
out_file <- paste0("data_raw/noaa_raw_", Sys.Date(), ".rds")
saveRDS(noaa_raw, out_file)

file.size(out_file) / 1e6 #size in MB
