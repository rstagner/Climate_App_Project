library(tidyverse)

#read climate source file
climate <- readRDS("data/climate_joined.rds")

#degree-day base temperature (from NWS, EIA): 65 F = 18.3 C (U.S Standard)
dd_base <- 18.3

#create columns for decades, and NASA/NOAA heating and cooling days
climate <- climate |> 
  mutate(decade = paste0(floor(year / 10) * 10, "s"),
         noaa_hdd = pmax(dd_base - noaa_mean, 0),
         nasa_hdd = pmax(dd_base - nasa_mean, 0),
         noaa_cdd = pmax(noaa_mean - dd_base, 0),
         nasa_cdd = pmax(nasa_mean - dd_base, 0)
  )

#check work
climate |>  count(decade)
climate |> 
  summarise(hdd = mean(noaa_hdd, na.rm = TRUE),
            cdd = mean(noaa_cdd, na.rm = TRUE),
            .by = city) |> 
  arrange(desc(cdd)) |> 
  print(n = 5)

#create heat wave data
#heat-wave threshold: each city's 90th percentile of daily max
#1991- 2020 normal period (climate normals are 30-year averages 
#that the World Meteorological Organization defines and NOAA publishes 
#for U.S. stations.)

#-----NOAA

#create heatwave thresholds for each city from noaa temp data
noaa_thresholds <- climate |> 
  filter(year >= 1991, year <= 2020) |> 
  summarize(noaa_hw_threshold = quantile(noaa_max, 0.90, na.rm = TRUE),
            .by = GEOID)
#join back to climate
climate <- climate |> 
  left_join(noaa_thresholds, by = "GEOID") |> 
  arrange(GEOID, date) |> 
  #is the day hotter than the city's threshold? missing days do not count
  mutate(noaa_hot = coalesce(noaa_max > noaa_hw_threshold, FALSE)) |> 
  #number each run of hot or not-hot days in a row, within each city
  mutate(run_id = consecutive_id(noaa_hot), .by = GEOID) |> 
  #how long is each run?
  mutate(run_length = n(), .by = c(GEOID, run_id)) |> 
  #a heat wave day is a hot day in a run of 2 or more
  mutate(noaa_heatwave = noaa_hot & run_length >= 2) |> 
  select(-run_id, -run_length)
  
#check work for NOAA
#average heat-wave days per city per year, by decade
climate |> 
  summarise(hw_days = sum(noaa_heatwave), .by = c(GEOID, year, decade)) |> 
  summarise(avg_hw_days = mean(hw_days), .by = decade)

#---------- NASA

#create heatwave thresholds for each city from nasa temp data
nasa_thresholds <- climate |> 
  filter(year >= 1991, year <= 2020) |> 
  summarize(nasa_hw_threshold = quantile(nasa_max, 0.90, na.rm = TRUE),
            .by = GEOID)

#join back to climate
climate <- climate |> 
  left_join(nasa_thresholds, by = "GEOID") |> 
  arrange(GEOID, date) |> 
  #is the day hotter than the city's threshold? missing days do not count
  mutate(nasa_hot = coalesce(nasa_max > nasa_hw_threshold, FALSE)) |> 
  #number each run of hot or not-hot days in a row, within each city
  mutate(run_id = consecutive_id(nasa_hot), .by = GEOID) |> 
  #how long is each run?
  mutate(run_length = n(), .by = c(GEOID, run_id)) |> 
  #a heat wave day is a hot day in a run of 2 or more
  mutate(nasa_heatwave = nasa_hot & run_length >= 2) |> 
  select(-run_id, -run_length)

#check work for NASA
#average heat-wave days per city per year, by decade
climate |> 
  summarise(hw_days = sum(nasa_heatwave), .by = c(GEOID, year, decade)) |> 
  summarise(avg_hw_days = mean(hw_days), .by = decade)

#save file
saveRDS(climate, "data/climate_daily.rds")
