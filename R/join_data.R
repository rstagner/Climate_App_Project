library(tidyverse)

#load the tidy data and the final city list
nasa <- readRDS("data/nasa_power_tidy.rds")
noaa <- readRDS("data/noaa_tidy.rds")
city_stations <- readRDS("data/city_stations.rds")

#fixed end date so recent unfinished NASA values aren't included
end_date <- as.Date("2026-10-31")

#select columns before join
nasa_join <- nasa |> 
  select(GEOID, date, nasa_mean, nasa_max, nasa_min)

noaa_join <- noaa |> 
  select(GEOID, date, station_id,
         noaa_mean = mean_temp, noaa_max = TMAX,
         noaa_min = TMIN, noaa_precip = PRCP)

#join, filter and add the city information

climate <- nasa_join |> 
  full_join(noaa_join, by = c("GEOID", "date")) |> 
  filter(date <= end_date) |> 
  left_join(city_stations |> select(GEOID, 
                                    city = CITY, 
                                    state = STATE, 
                                    region = REGION, 
                                    lat = LAT, 
                                    lon = LON),
            by = "GEOID") |> 
  mutate(year = year(date),
         month = month(date))

#check work
nrow(climate) #should be about 645,700

n_distinct(climate$GEOID) #shoudd be 48

climate |> count(GEOID, date) |> #0, no duplicate days
  filter(n > 1) |> 
  nrow()

climate |> filter(is.na(city)) |> #0, every row has a city
  nrow()

#this is also a preview of the satellite vs. station
summary(select(climate, nasa_mean, noaa_mean)) #both should be similar, but not the same

saveRDS(climate, "data/climate_joined.rds")
file.size("data/climate_joined.rds") / 1e6   #size in MB
