#Load library
library(tidyverse)

# load config
source("R/00_config.R")

# Load rds for 11/03/2026--------------
nasa_raw_list <- readRDS("data_raw/nasa_raw_11-03-2026.rds")

# =========================================================================
# Tidying pipeline
# =========================================================================

# To make sure raw list has the correct city names assigned from your config
names(nasa_raw_list) <- city_coords$CITY

nasa_tidy <- imap_dfr(nasa_raw_list, function(city_json, city_name) {
  
  # 1. Skip if a city's JSON response is empty or failed
  if (is.null(city_json)) return(NULL)
  
  params_list <- city_json$properties$parameter
  
  param_tibbles <- imap(params_list, function(date_vals, param_name) {
    tibble(
      DATE_STR = names(date_vals),
      VALUE = unlist(date_vals)
    ) |>
      mutate(ELEMENT = param_name)
  })
  
  # 2. Combine parameters and ensure CITY is a character string name
  bind_rows(param_tibbles) |>
    pivot_wider(names_from = ELEMENT, values_from = VALUE) |>
    mutate(CITY = as.character(city_name))
  
}) |>
  
  # 3. Handle missing data (-999 fill values)
  mutate(across(where(is.numeric), ~ ifelse(. <= -999, NA, .))) |>
  
  # 4. Join back lat/lon using character matching
  left_join(
    city_coords |> mutate(CITY = as.character(CITY)) |> select(CITY, LAT, LON), 
    by = "CITY"
  ) |>
  
  # 5. Finalize 
  transmute(
    date        = as.Date(DATE_STR, format = "%Y%m%d"),
    city        = CITY,
    lat         = LAT,
    lon         = LON,
    mean_temp   = T2M,
    max_temp    = T2M_MAX,
    min_temp    = T2M_MIN
  )

# data quality check ----------

# 1. make sure all cities intended are included
unique_cities_nasa <- unique(nasa_tidy$city)
cat("\n--- Unique Cities List ---\n")
print(unique_cities_nasa)

# 2. Create a summary of observations for each city and year
city_year_summary_nasa <- nasa_tidy |>
  mutate(year = format(date, "%Y")) |>
  count(city, year, name = "observation_count")
cat("\n--- Observations per City and Year ---\n")
print(city_year_summary_nasa, n = 20) # Prints the first 20 rows; adjust 'n' as needed

# Save rds and check file size
saveRDS(nasa_tidy, "data/nasa_tidy.rds")
file.size("data/nasa_tidy.rds")




