library(tidyverse)

#load the raw data
raw_file <- jsonlite::fromJSON("../data_raw/census_ua_pop_2026-11-15.json")

colnames(raw) <- raw[1, ] #row one has column names

ua_pop <- as_tibble(raw[-1, ])

ua_pop_clean <- ua_pop |> 
  select(population = P1_001N,
         GEOID = `urban area`) |> 
  mutate(GEOID = as.character(GEOID),
         population = as.numeric(population))

glimpse(ua_pop_clean)
