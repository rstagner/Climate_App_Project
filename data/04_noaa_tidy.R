#load library

library (tidyverse)

# load config
source("R/00_config.R")

# Load the raw NOAA snapshot list
noaa_raw_list <- readRDS("data_raw/noaa_raw_11-03-2026.rds")

# Combine raw list, clean, and pivot
noaa_wide <- bind_rows(noaa_raw_list) |>
  rename(DATE_STR = X2, ELEMENT = X3, VALUE = X4) |>
  filter(ELEMENT %in% c("TAVG", "TMAX", "TMIN")) |>
  mutate(
    VALUE = as.numeric(VALUE) / 10, # NOAA temp values are stored in tenths of degrees C
    VALUE = ifelse(VALUE == -999.9, NA, VALUE)
  ) |>
  select(CITY, STATION, DATE_STR, ELEMENT, VALUE) |>
  pivot_wider(names_from = ELEMENT, values_from = VALUE)


# Final standardization, fallback mean calculation, and transmute
noaa_tidy <- noaa_wide |>
  left_join(city_coords |> select(CITY, LAT, LON), by = "CITY") |>
  mutate(
    # Fallback logic: Use TAVG if present, otherwise average TMAX and TMIN
    calculated_mean = case_when(
      !is.na(TAVG) ~ TAVG,
      !is.na(TMAX) & !is.na(TMIN) ~ (TMAX + TMIN) / 2,
      TRUE ~ NA_real_
    ),
    date = as.Date(DATE_STR, format = "%Y%m%d")
  ) |>
  filter(date >= as.Date("1990-01-01") & date <= Sys.Date()) |>
  transmute(
    date      = as.Date(DATE_STR, format = "%Y%m%d"),
    city      = CITY,
    lat       = LAT,
    lon       = LON,
    mean_temp = calculated_mean,
    max_temp  = TMAX,
    min_temp  = TMIN
  )

