# data/tidy_nasa_power.R
# Grab the newest raw NASA POWER file from data_raw/, check it against
# data/city_stations.rds, fix dates and -999 missing numbers, and save a clean
# table to data/nasa_power_tidy.rds.
# Run from the main project folder (Climate_App_Project).

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

# Keep GEOID stored the same way it is in data/city_stations.rds 
cast_to_class <- function(x, target_class) {
  if (target_class == "factor") {
    factor(as.character(x))
  } else if (target_class == "integer") {
    as.integer(x)
  } else if (target_class == "numeric" || target_class == "double") {
    as.numeric(x)
  } else if (target_class == "character") {
    as.character(x)
  } else {
    methods::as(x, target_class)
  }
}
