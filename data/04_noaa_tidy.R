#load library

library (tidyverse)

# load config
source("R/00_config.R")

#### Load rds for 11/03/2026---------------
noaa_raw_list <- readRDS("data_raw/noaa_raw_11-03-2026.rds")

#### Combine raw list, clean, and pivot---------------
noaa_wide <- bind_rows(noaa_raw_list) |>
  rename(DATE_STR = X2, ELEMENT = X3, VALUE = X4) |>
  filter(ELEMENT %in% c("TAVG", "TMAX", "TMIN")) |>
  mutate(
    VALUE = as.numeric(VALUE) / 10, # NOAA temp values are stored in tenths of degrees C
    VALUE = ifelse(VALUE == -999.9, NA, VALUE)
  ) |>
  select(CITY, STATION, DATE_STR, ELEMENT, VALUE) |>
  pivot_wider(names_from = ELEMENT, values_from = VALUE)


# Final standardization, fallback mean calculation, and transmute------------
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

# data quality check----------------
# 1. make sure all cities intended are included
unique_cities_noaa <- unique(noaa_tidy$city)
cat("\n--- Unique Cities List ---\n")
print(unique_cities_noaa)

# 2. Create a summary of observations for each city and year
city_year_summary_noaa <- noaa_tidy |>
  mutate(year = format(date, "%Y")) |>
  count(city, year, name = "observation_count")
cat("\n--- Observations per City and Year ---\n")
print(city_year_summary_noaa, n = 20) # Prints the first 20 rows; adjust 'n' as needed

# 3. tibble preview 
cat("\n--- NOAA Tidy Tibble Preview ---\n")
print(noaa_tidy, n = 10)

# Save rds and check file size-------------
saveRDS(noaa_tidy, "data/noaa_tidy.rds")
file.size("data/nasa_tidy.rds")
