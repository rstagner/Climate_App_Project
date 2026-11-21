library(tidyverse)

start_date <- as.Date("1990-01-01")
end_date <- as.Date("2026-10-31") #fixed cutoff so results don't change
days_possible <- as.numeric(end_date - start_date) + 1

noaa_tidy <- readRDS("data/noaa_tidy.rds")

#count days per station
completeness <- noaa_tidy |> 
  filter(date >= start_date & date <= end_date) |> 
  summarise(temp_days = sum(!is.na(TMAX) & !is.na(TMIN)),
            precip_days = sum(!is.na(PRCP)),
            .by = c(GEOID, CITY, station_id)) |> 
  mutate(temp_pct = temp_days / days_possible,
         precip_pct = precip_days / days_possible,
         meets_90 = temp_pct >= 0.90)

#check for completeness
completeness |>  arrange(temp_pct) |> print(n = Inf)
completeness |> count(meets_90)

#investigate failing cities
failing <- completeness |> filter(!meets_90) |> pull(CITY)

noaa_tidy |> 
  filter(CITY %in% failing, date <= end_date) |> 
  mutate(year = year(date)) |> 
  summarise(days = sum(!is.na(TMAX) & !is.na(TMIN)), .by = c(CITY, year)) |> 
  pivot_wider(names_from = CITY, values_from = days) |> 
  arrange(year) |> 
  print(n = Inf)
