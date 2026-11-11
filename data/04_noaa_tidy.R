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


