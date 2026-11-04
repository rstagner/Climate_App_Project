#Load libraries

library(tidyverse)
library(httr2)
library(jsonlite)

# Load central configuration (coordinates & parameters)
source("R/00_config.R")

# Fetch function with error handling
fetch_nasa_raw <- function(CITY, LAT, LON) {
  base_url <- "https://power.larc.nasa.gov/api/temporal/daily/point"
  message(sprintf("Fetching raw NASA data for %s...", CITY))
  
  tryCatch({
    req <- request(base_url) |>
      req_url_query(
        parameters = NASA_PARAMETERS,
        community  = "re",
        longitude  = LON,
        latitude   = LAT,
        start      = START_DATE,
        end        = CUTOFF_DATE,
        format     = "JSON"
      ) |>
      req_timeout(60)
    
    resp <- req |> req_perform()
    
    # FIXED: Added 'resp |>' so the response object is properly passed
    resp |> resp_body_string() |> fromJSON(simplifyVector = FALSE)
    
  }, error = function(e) {
    message(sprintf("  -> Failed for %s: %s", CITY, e$message))
    NULL
  })
}

# Execute batch fetch across all 48 cities
nasa_raw_list <- pmap(
  city_coords |> select(CITY, LAT, LON),
  fetch_nasa_raw
)
names(nasa_raw_list) <- city_coords$CITY