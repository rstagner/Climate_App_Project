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

# Pick the newest raw file on its own 
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

# Turn text dates into real dates, pull out year and month, and swap -999
# missing-data codes to NA

# Helper to turn raw NASA POWER temperature columns (<= -999 is missing) into clean numeric values
clean_nasa_temp <- function(x) {
  val <- suppressWarnings(as.numeric(x))
  ifelse(!is.na(val) & val <= -999, NA_real_, val)
}

# Map T2M -> nasa_mean, T2M_MAX -> nasa_max, and T2M_MIN -> nasa_min while keeping
# GEOID matched to city_stations 
nasa_tidy <- checked_tbl |>
  mutate(
    GEOID     = cast_to_class(GEOID, geoid_class),
    city      = if ("CITY" %in% names(checked_tbl)) as.character(CITY) else ref_city,
    state     = if ("STATE" %in% names(checked_tbl)) as.character(STATE) else ref_state,
    date      = as.Date(as.character(DATE_YYYYMMDD), format = "%Y%m%d"),
    year      = as.integer(format(date, "%Y")),
    month     = as.integer(format(date, "%m")),
    nasa_mean = if ("T2M" %in% names(checked_tbl)) clean_nasa_temp(T2M) else NA_real_,
    nasa_max  = if ("T2M_MAX" %in% names(checked_tbl)) clean_nasa_temp(T2M_MAX) else NA_real_,
    nasa_min  = if ("T2M_MIN" %in% names(checked_tbl)) clean_nasa_temp(T2M_MIN) else NA_real_,
    pulled_date = pull_dt
  ) |>
  select(
    GEOID,
    city,
    state,
    date,
    year,
    month,
    nasa_mean,
    nasa_max,
    nasa_min,
    pulled_date
  ) |>
  arrange(GEOID, date)

# Check that dates worked, no city-day is doubled up, and print a quick summary
# of how many cities are there and how many missing values were swapped
stopifnot(
  !anyNA(nasa_tidy$date),
  nrow(nasa_tidy) == nrow(distinct(nasa_tidy, GEOID, date))
)

n_cities_present <- n_distinct(nasa_tidy$GEOID)
missing_cities   <- setdiff(stations_ref$GEOID, unique(nasa_tidy$GEOID))

if (length(missing_cities) > 0) {
  message("[Warning] Missing cities (", length(missing_cities), " of 48): ",
          paste(as.character(missing_cities), collapse = ", "))
}

na_summary <- nasa_tidy |>
  summarise(
    na_mean = sum(is.na(nasa_mean)),
    na_max  = sum(is.na(nasa_max)),
    na_min  = sum(is.na(nasa_min))
  )

# check why na_summmary returned 144/144/144 instead of 0/0/0
na_rows <- nasa_tidy |>
  filter(is.na(nasa_mean) | is.na(nasa_max) | is.na(nasa_min))

# Check which cities have NAs
na_rows |>
  count(GEOID, city, state, sort = TRUE)

# Check which years/dates they fall in
na_rows |>
  count(year, sort = TRUE)

na_rows |>
  select(GEOID, city, state, date, nasa_mean, nasa_max, nasa_min) |>
  head(10)

# Note: NAs starting around 2026-11-17 come from recent dates at the tail of the
# pull where NASA POWER values aren't finalized yet (144 rows across mean/max/min).
# Keep them as NA rather than dropping rows or leaving -999s in the data.

# Save the clean table to data/ and print how big the file is and how many rows
# it has before anything gets pushed or shared
dir.create("data", showWarnings = FALSE, recursive = TRUE)
out_tidy_path <- "data/nasa_power_tidy.rds"
saveRDS(nasa_tidy, out_tidy_path)

size_mb <- file.info(out_tidy_path)$size / (1024 * 1024)
message(sprintf("[Success] Saved '%s' (%.2f MB | %s rows)",
                out_tidy_path, size_mb, format(nrow(nasa_tidy), big.mark = ",")))
