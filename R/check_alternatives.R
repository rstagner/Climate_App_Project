library(tidyverse)

#load dataframes
candidates <- readRDS("data/station_candidates.rds")
completeness <- readRDS("data/noaa_completeness.rds")

#check for alternative stations
failing_ids <- completeness |> filter(!meets_90) |> pull(GEOID)
current_stations <- completeness |> pull(station_id)

alternatives <- candidates |> 
  filter(GEOID %in% failing_ids,
         !station_id %in% current_stations) |>   #skip stations already in use
  select(GEOID, city, station_id, station_name, is_wban, dist_km) |> 
  arrange(city, desc(is_wban), dist_km)

alternatives |> print(n = Inf)
alternatives |> count(city)

#same column names and download function as the pull script
noaa_cols <- c("station_id", "date", "element", "value",
               "mflag", "qflag", "sflag", "obs_time")

fetch_station <- function(id) {
  url <- paste0("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/", id, ".csv.gz")
  message("Fetching ", id)
  tryCatch(
    read_csv(url, col_names = noaa_cols, col_types = cols(.default = "c")) |> 
      filter(element %in% c("TMAX", "TMIN", "PRCP"),
             date >= "19900101"),
    error = function(e) {
      message("  failed: ", e$message)
      NULL
    }
  )
}

#download the alternatives and save the raw data
alt_raw <- map(alternatives$station_id, fetch_station) |> list_rbind()
saveRDS(alt_raw, paste0("data_raw/noaa_alt_raw_", Sys.Date(), ".rds"))

#same period as the main completeness check
start_date    <- as.Date("1990-01-01")
end_date      <- as.Date("2026-10-31")
days_possible <- as.numeric(end_date - start_date) + 1

#completeness for each alternative
alt_completeness <- alt_raw |> 
  filter(is.na(qflag)) |> 
  mutate(date  = as.Date(date, format = "%Y%m%d"),
         value = as.numeric(value)) |> 
  filter(value != -9999) |> 
  select(station_id, date, element, value) |> 
  pivot_wider(names_from = element, values_from = value) |> 
  filter(date >= start_date & date <= end_date) |> 
  summarise(temp_days   = sum(!is.na(TMAX) & !is.na(TMIN)),
            precip_days = sum(!is.na(PRCP)),
            .by = station_id) |> 
  mutate(temp_pct   = temp_days / days_possible,
         precip_pct = precip_days / days_possible,
         meets_90   = temp_pct >= 0.90) |> 
  left_join(alternatives, by = "station_id") |> 
  arrange(city, desc(meets_90), desc(is_wban), dist_km)

alt_completeness |> 
  select(city, station_name, is_wban, dist_km, temp_pct, precip_pct, meets_90) |> 
  print(n = Inf)
