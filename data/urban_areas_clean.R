library(tidyverse)

#read Gazetteer Data for urban areas

urban_areas <- read_delim(
                          "../data_raw/2026_Gaz_ua_national.txt",
                          delim = "|",
                          col_name = TRUE,
                          trim_ws = TRUE
                        ) |> 
  #select relevant columns
  select(GEOID = GEOID,
         name = NAME, 
         lat = INTPTLAT, 
         long = INTPTLONG) |> 
  mutate(GEOID = as.character(GEOID),
         name = str_remove(name, " Urban Area$"), #clean extra text in name
         name = if_else(name == "Kailua (Hawaii County), HI", #account for two Kailuas
                        "Kailua-Kona, HI",
                        name)
        ) |> 
  #split name into "City" and "State" columns
  separate_wider_delim(
                     cols = "name",
                     delim = ", ",
                     names = c("city", "state")
                     ) |> 
  #remove extra characters in city and state
  mutate(city = str_trim(str_remove(city, "--.*|/.*|\\(.*")),
         state = str_remove(state, "--.*")
         )
