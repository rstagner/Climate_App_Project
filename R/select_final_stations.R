library(tidyverse)

candidate_completeness <- readRDS("data/candidate_completeness.rds")

#best passing station for each city: Weather Service first, then nearest
best_stations <- candidate_completeness |> 
  filter(meets_90) |> 
  arrange(desc(is_wban), dist_km) |> 
  slice_head(n = 1, by = GEOID)

#check if any station is picked for two cities
best_stations |> count(station_id) |> filter(n > 1)

#keep the largest 12 cities per region
city_stations <- best_stations |> 
  slice_max(population, n = 12, by = region)

#check work
nrow(city_stations) #should be 48
city_stations |>  count(region) #should be 12 per region
city_stations |>  count(is_wban) #check for which stations are Weather Service

#which cities changed from the original list
city_stations |> 
  filter(!city %in% readRDS("data/top_cities.rds")$city) |> 
  select(region, city, station_name, temp_pct)

#rename to match the config and keep the final columns
city_stations <- city_stations |> 
  select(REGION = region,
         CITY = city,
         STATE = state,
         LAT = lat, LON = long,
         GEOID,
         POP = population,
         station_info_id = station_id,
         station_name, station_state, station_lat, station_long,
         dist_km, is_wban, temp_pct, precip_pct)

saveRDS(city_stations, "data/city_stations.rds")
write_csv(city_stations, "data/city_stations.csv")
  