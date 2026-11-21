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

# Pick the newest raw file on its own instead of typing a date by hand
raw_files <- list.files("data_raw", pattern = "^nasa_power_raw_.*\\.rds$", full.names = TRUE)
stopifnot(length(raw_files) > 0)

latest_raw_path <- sort(raw_files, decreasing = TRUE)[1]
raw_obj         <- readRDS(latest_raw_path)
stopifnot(is.list(raw_obj), is.data.frame(raw_obj$combined))

# Read the current 48-city list and carry GEOID through 

stations_path <- "data/city_stations.rds"
stopifnot(file.exists(stations_path))

city_stations <- readRDS(stations_path)
geoid_class   <- class(city_stations$GEOID)[1]
stopifnot(nrow(city_stations) == 48)

stations_ref <- city_stations |>
  mutate(
    GEOID      = cast_to_class(GEOID, geoid_class),
    ref_city   = as.character(CITY),
    ref_state  = as.character(STATE),
    ref_lat_r4 = round(as.numeric(LAT), 4),
    ref_lon_r4 = round(as.numeric(LON), 4)
  ) |>
  select(GEOID, ref_city, ref_state, ref_lat_r4, ref_lon_r4)

# check if pull matches the current city list and locations
checked_tbl <- as_tibble(raw_obj$combined) |>
  mutate(GEOID = cast_to_class(GEOID, geoid_class)) |>
  left_join(stations_ref, by = "GEOID")

unmatched_geoids <- checked_tbl |>
  filter(is.na(ref_city)) |>
  distinct(GEOID) |>
  pull(GEOID)

if (length(unmatched_geoids) > 0) {
  stop("Raw file has IDs not in data/city_stations.rds (old pull?): ",
       paste(as.character(unmatched_geoids), collapse = ", "))
}

if (all(c("query_lat", "query_lon") %in% names(checked_tbl))) {
  coord_mismatches <- checked_tbl |>
    mutate(
      raw_lat_r4 = round(as.numeric(query_lat), 4),
      raw_lon_r4 = round(as.numeric(query_lon), 4)
    ) |>
    filter(raw_lat_r4 != ref_lat_r4 | raw_lon_r4 != ref_lon_r4) |>
    distinct(GEOID, ref_city, ref_state)
  
  if (nrow(coord_mismatches) > 0) {
    warning("Locations don't match data/city_stations.rds for: ",
            paste(paste(coord_mismatches$ref_city, coord_mismatches$ref_state, sep = ", "), collapse = "; "))
  }
}

pull_dt <- if (!is.null(raw_obj$pulled_date)) {
  as.Date(raw_obj$pulled_date)
} else if ("pulled_date" %in% names(checked_tbl)) {
  as.Date(checked_tbl$pulled_date[[1]])
} else {
  Sys.Date()
}
