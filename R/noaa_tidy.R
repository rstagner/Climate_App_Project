library(tidyverse)

#load files
noaa_raw <- readRDS("data_raw/noaa_raw_2026-11-21.rds")
city_stations <- readRDS("data/city_stations.rds")

#clean data
noaa_clean <- noaa_raw |> 
  filter(is.na(qflag)) |>  #remove NOAA quality-flagged data
  mutate(date = as.Date(date, format = "%Y%m%d"),
         value = as.numeric(value)) |> 
  filter(value != -9999) |> #remove missing values
  mutate(value = value / 10) #convert from tenths to °C and mm

#pivot wider so that each date has it's own row with TMAX, TMIN, and PRCP
noaa_pivot <- noaa_clean |> 
  select(station_id, date, element, value) |> 
  pivot_wider(names_from = element,
              values_from = value)

glimpse(noaa_pivot) #check work

#add a mean temperature column
noaa_pivot <- noaa_pivot |> 
  mutate(mean_temp = (TMAX + TMIN) / 2)

#select relevant columns from the city_stations file
city_info <- city_stations |> 
  select(station_info_id, GEOID, CITY, STATE, REGION, LAT, LON)

#join dataframes by the station id
noaa_tidy <- noaa_pivot |> 
  left_join(city_info, by = c("station_id" = "station_info_id"))

#check that every row matches a city
noaa_tidy |> 
  filter(is.na(GEOID)) |> nrow()

#check the dataframe
glimpse(noaa_tidy)

#check for 48 cities, dates from 1990 onward
n_distinct(noaa_tidy$GEOID)
range(noaa_tidy$date)

#data check - are values reasonable? 
summary(select(noaa_tidy, TMAX, TMIN, PRCP))

#check for days where min is above max (other data issue)
noaa_tidy |> 
  filter(TMIN > TMAX) |> nrow()

#check extreme values to real events 
#top temperatures and rainfall, with the city and date
noaa_tidy |> slice_max(TMAX, n = 3) |> select(CITY, date, TMAX)
noaa_tidy |> slice_min(TMIN, n = 3) |> select(CITY, date, TMIN)
noaa_tidy |> slice_max(PRCP, n = 3) |> select(CITY, date, PRCP)

saveRDS(noaa_tidy, "data/noaa_tidy.rds")
