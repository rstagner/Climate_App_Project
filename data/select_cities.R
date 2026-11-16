library(tidyverse)

urban_areas <- readRDS("urban_areas_clean.rds")
ua_pop <- readRDS("ua_pop_clean.rds")

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

top_cities <- urban_join |> 
  slice_max(population, n = 12, by = region)

top_cities |> count(region)                  
