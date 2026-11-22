library(tidyverse)

#load the raw candidate data and the city-station pairs
noaa_raw   <- readRDS("data_raw/noaa_raw_candidates_2026-11-22.rds")
candidates <- readRDS("data/station_candidates.rds")

#period for the 90% rule (fixed cutoff)
start_date    <- as.Date("1990-01-01")
end_date      <- as.Date("2026-10-31")
days_possible <- as.numeric(end_date - start_date) + 1

#completeness for each station
station_completeness <- noaa_raw |> 
  filter(is.na(qflag)) |> #filter out quality flagged stations
  mutate(date  = as.Date(date, format = "%Y%m%d"),
         value = as.numeric(value)) |> 
  filter(value != -9999) |> #filter out invalid 
  select(station_id, date, element, value) |> 
  pivot_wider(names_from = element, values_from = value) |> 
  filter(date >= start_date & date <= end_date) |> 
  summarise(temp_days   = sum(!is.na(TMAX) & !is.na(TMIN)),
            precip_days = sum(!is.na(PRCP)),
            .by = station_id) |> 
  mutate(temp_pct   = temp_days / days_possible,
         precip_pct = precip_days / days_possible,
         meets_90   = temp_pct >= 0.90) 

#join for check
candidate_completeness <- left_join(candidates, station_completeness, by = "station_id")

#save the file
saveRDS(candidate_completeness, "data/candidate_completeness.rds")

#check the work
nrow(candidate_completeness)  #should be 142, one per pair
candidate_completeness |> filter(is.na(temp_pct)) |> nrow()   #should be 0
station_completeness |> count(meets_90)   #how many of the 142 pass

#cities with no passing station
candidate_completeness |> 
  summarise(any_pass = any(meets_90), .by = c(region, city)) |> 
  filter(!any_pass)



