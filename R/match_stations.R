library(tidyverse)

#read NOAA station reference files
stations <- read_fwf(
  "data_raw/ghcnd-stations_2026-11-16.txt",
  col_positions = fwf_cols(
  station_id = c(1, 11),
  lat = c(13, 20),
  long = c(22,30),
  elev = c(32, 37),
  state = c(39,40),
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
