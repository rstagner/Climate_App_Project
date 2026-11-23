library(tidyverse)

#read NOAA station reference files
stations <- read_fwf(
  "data_raw/ghcnd-stations_2026-11-16.txt",
  col_positions = fwf_cols(
    station_id = c(1, 11),
    lat = c(13, 20),
    long = c(22, 30),
    elev = c(32, 37),
    state = c(39, 40),
    name = c(42, 71)
  )
)

inventory <- read_fwf(
  "data_raw/ghcnd-inventory_2026-11-16.txt",
  col_positions = fwf_cols(
    station_id = c(1, 11),
    lat = c(13, 20),
    long = c(22,30),
    element = c(32, 35),
    first_year = c(37,40),
    last_year = c(42, 45)
  )
)  
#check work
glimpse(stations)  
glimpse(inventory)

#filter to only U.S. States

stations <- stations |> 
  filter(state %in% c(state.abb, "DC")) 

#confirm
states <- unique(stations$state)
print(states)

#filter for stations within the time range, and that has all three variables of interest
qualifying_ids <- inventory |> 
  filter(
    element %in% c("TMAX", "TMIN", "PRCP"),
    first_year <= 1990,
    last_year == 2026
  ) |> 
  count(station_id) |> 
  filter(n == 3)

#inner join for only those qualifying stations in the U.S.
us_stations <- inner_join(stations, qualifying_ids, by = "station_id")

#check work
nrow(us_stations)
us_stations |> count(state) |> print(n = Inf)
us_stations |> filter(str_detect(name, "NEW YORK|REAGAN|CHICAGO OHARE"))

stations |> filter(state == "DC")

#checking why DC isn't in the list
inventory |> 
  filter(station_id %in% (stations |> filter(state == "DC") |> pull(station_id)),
         element %in% c("TMAX", "TMIN", "PRCP")) |> 
  print(n = Inf)

#using trigonometry to calculate the closest station to each city
#great-circle distance in km using the haversine formula (Sinnott, 1984)
#function drafted with assistance from Claude (Anthropic); 
#validated against geosphere::distHaversine()

haversine_km <- function(lat1, long1, lat2, long2) {
  to_rad <- pi / 180
  dlat <-(lat2 - lat1) * to_rad
  dlong <- (long2 - long1) * to_rad
  a <- sin(dlat / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlong / 2)^2
  2 * 6371 * asin(sqrt(a)) #6371 is the Earth's radius
}

#test the function for New York to LA should be ~3935 km
haversine_km(40.71, -74.01, 34.05, -118.24)

#rename columns before join
station_lookup <- us_stations |> 
  select(
    station_id,
    station_lat = lat,
    station_long = long,
    station_state = state,
    station_name = name
  )

#load the pool of 60 cities (top 15 per region)
city_pool <- readRDS("data/city_pool.rds") 

#pair every city with every station and keep all stations within 25 km
candidates <- city_pool |>
  cross_join(station_lookup) |>  #every city-station pair
  mutate(
    dist_km = haversine_km(lat, long, station_lat, station_long)) |>
  filter(dist_km <= 25) |> #only stations within 25 km
  mutate(is_wban = str_starts(station_id, "USW")) #flag Weather Service Stations

#save every candidate; completeness is checked in R/station_completeness.R
#and the final station for each city is picked in R/select_final_stations.R
saveRDS(candidates, "data/station_candidates.rds")

#check work: how many candidate stations each city has
candidates |> count(city) |> arrange(n) |> print(n = Inf)

#check work: cities with at least one candidate, and stations to download
n_distinct(candidates$GEOID)
n_distinct(candidates$station_id)

#check work: pool cities with no qualifying station within 25 km
city_pool |>
  filter(!GEOID %in% candidates$GEOID) |>
  select(region, city, state)

pool <- readRDS("data/city_pool.rds")
nrow(pool)
pool |> count(region)
