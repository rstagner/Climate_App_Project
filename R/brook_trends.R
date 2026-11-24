library(tidyverse)

# ------------------------------------------------------------------------------
# Load annual climate data and city lookup
# ------------------------------------------------------------------------------
climate_annual <- readRDS("data/climate_annual.rds")
city_stations  <- readRDS("data/city_stations.rds")

if (all(c("CITY", "STATE", "GEOID") %in% names(city_stations))) {
  city_list <- city_stations |>
    mutate(label = paste0(CITY, ", ", STATE)) |>
    arrange(label)
  city_choices <- setNames(as.character(city_list$GEOID), city_list$label)
} else {
  city_choices <- NULL
}
