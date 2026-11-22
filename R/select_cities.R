library(tidyverse)

urban_areas <- readRDS("data/urban_areas_clean.rds")
ua_pop <- readRDS("data/ua_pop_clean.rds")

#join files into one
urban_join <- left_join(urban_areas, ua_pop, by = "GEOID")

#confirm it worked
urban_join |> filter(is.na(population))

#create a tibble of census regions
regions <- tibble(state = state.abb, region = as.character(state.region)) |> 
                  add_row(state = "DC", region = "South") |> 
                  mutate(region = if_else(region == "North Central", "Midwest", region))
#check work
regions |> count(region)

#join the two to include regions
urban_join <- left_join(urban_join, regions, by = "state")

#select the top 12 most populous cities in each census region
city_pool <- urban_join |> 
  slice_max(population, n = 15, by = region)

#check work
nrow(city_pool) #should be 60
city_pool|> count(region)   #15 in each region
city_pool |> 
  filter(!city %in% readRDS("data/top_cities.rds")$city) |> 
  select(region, city, state, population) #12 new spare candidates

#save RDS file
saveRDS(top_cities, "data/city_pool.rds")

write_csv(top_cities, "data/city_pool.csv")
