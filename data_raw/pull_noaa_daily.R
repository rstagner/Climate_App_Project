library(tidyverse)

#load the 60 matched stations with 25 km of the 60 pool cities
candidates <- readRDS("data/station_candidates.rds")
station_ids <- unique(candidates$station_id)
length(station_ids) #should be 141

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

#download every candidate station
noaa_list <- map(station_ids, fetch_station)
names(noaa_list) <- station_ids

#report any failures
failed <- names(noaa_list)[map_lgl(noaa_list, is.null)]
if (length(failed) >0) message("No data for: ", paste(failed, collapse = ", "))

#combine all stations into one table
noaa_raw <- list_rbind(noaa_list)

#check work
n_distinct(noaa_raw$station_id) #should be 141, minus failures
noaa_raw |> count(element)

#save with download date
out_file <- paste0("data_raw/noaa_raw_candidates_", Sys.Date(), ".rds")
saveRDS(noaa_raw, out_file, compress = "xz")

file.size(out_file) / 1e6 #size in MB
