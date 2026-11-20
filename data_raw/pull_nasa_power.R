# data_raw/pull_nasa_power.R
# Pull daily NASA POWER data for cities in data/city_stations.rds with preliminary table shaping.
# Run from the project root directory (Climate_App_Project).
# Writes a single dated .rds file in data_raw/ with the pull date saved inside.

library(dplyr)
library(purrr)
library(httr2)
library(jsonlite)

# Pull settings - adjust before running
start_date <- "19900101"
end_date   <- format(Sys.Date(), "%Y%m%d")
community  <- "AG"
parameters <- c("T2M", "T2M_MAX", "T2M_MIN")
pause_sec  <- 0.25
max_cities <- NULL # Set to e.g. 3 for a quick preliminary test; NULL pulls all 48

# Match whatever class GEOID has in city_stations.rds
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

# Load city_stations and validate the 48-city source table
stations_path <- "data/city_stations.rds"
stopifnot(file.exists(stations_path))

city_stations <- readRDS(stations_path)
geoid_class   <- class(city_stations$GEOID)[1]

stopifnot(
  nrow(city_stations) == 48,
  !anyNA(city_stations$GEOID),
  !anyNA(city_stations$LAT),
  !anyNA(city_stations$LON)
)

city_stations <- city_stations |>
  mutate(
    GEOID    = cast_to_class(GEOID, geoid_class),
    city_key = paste(as.character(GEOID), CITY, STATE, sep = "_")
  )

pull_stations <- if (!is.null(max_cities)) {
  head(city_stations, max_cities)
} else {
  city_stations
}
