# =========================================================================
# PROJECT CONFIGURATION 
# =========================================================================
# Description: Centralized configuration file providing spatial coordinates 
#              and station IDs for data pipelines.
# =========================================================================

#Load library
library(tidyverse)

# -------------------------------------------------------------------------
# 1. GLOBAL TIMEFRAME & API SETTINGS
# -------------------------------------------------------------------------
START_DATE      <- "19900101"

# Static cutoff date (YYYYMMDD format) for your project snapshot
CUTOFF_DATE     <- format(Sys.Date(), "%Y%m%d")

# NASA POWER variables to request
NASA_PARAMETERS <- "T2M,T2M_MAX,T2M_MIN,PRECTOTCORR,RH2M,PS,WS2M"

# -------------------------------------------------------------------------
# 2. MASTER 48-CITY REGIONAL CONFIGURATION TABLE
# -------------------------------------------------------------------------
city_coords <- tibble::tribble(
  ~REGION,     ~CITY,                  ~STATE, ~LAT,       ~LON,       ~station_info_id,
  
  # --- NORTHEAST REGION ---
  "Northeast", "New York",             "NY",   40.7128,    -74.0060,   "USW00094728",
  "Northeast", "Philadelphia",       "PA",   39.9526,    -75.1652,   "USW00013739",
  "Northeast", "Hempstead",          "NY",   40.7062,    -73.6187,   "USW00094789",
  "Northeast", "Boston",             "MA",   42.3601,    -71.0589,   "USW00014739",
  "Northeast", "Brookhaven",         "NY",   40.8534,    -72.9554,   "USW00094789",
  "Northeast", "Islip",              "NY",   40.7296,    -73.2012,   "USW00014792",
  "Northeast", "Newark",             "NJ",   40.7357,    -74.1724,   "USW00014734",
  "Northeast", "Pittsburgh",         "PA",   40.4406,    -79.9959,   "USW00094823",
  "Northeast", "Jersey City",        "NJ",   40.7178,    -74.0431,   "USW00014734",
  "Northeast", "Oyster Bay",         "NY",   40.8701,    -73.5354,   "USW00094789",
  "Northeast", "Buffalo",            "NY",   42.8864,    -78.8784,   "USW00014733",
  "Northeast", "North Hempstead",    "NY",   40.7937,    -73.6854,   "USW00094789",
  
  # --- MIDWEST REGION ---
  "Midwest",   "Chicago",            "IL",   41.8781,    -87.6298,   "USW00094846",
  "Midwest",   "Columbus",           "OH",   39.9612,    -82.9988,   "USW00014821",
  "Midwest",   "Indianapolis",       "IN",   39.7684,    -86.1581,   "USW00093819",
  "Midwest",   "Detroit",            "MI",   42.3314,    -83.0458,   "USW00094847",
  "Midwest",   "Milwaukee",          "WI",   43.0389,    -87.9065,   "USW00014839",
  "Midwest",   "Kansas City",        "MO",   39.0997,    -94.5786,   "USW00003947",
  "Midwest",   "Omaha",              "NE",   41.2565,    -95.9345,   "USW00014942",
  "Midwest",   "Minneapolis",        "MN",   44.9778,    -93.2650,   "USW00014922",
  "Midwest",   "Wichita",            "KS",   37.6872,    -97.3301,   "USW00003928",
  "Midwest",   "Cleveland",          "OH",   41.4993,    -81.6944,   "USW00014820",
  "Midwest",   "Cincinnati",         "OH",   39.1031,    -84.5120,   "USW00093814",
  "Midwest",   "St. Paul",           "MN",   44.9537,    -93.0900,   "USW00014922",
  
  # --- SOUTH REGION ---
  "South",     "Houston",            "TX",   29.7604,    -95.3698,   "USW00012918",
  "South",     "San Antonio",        "TX",   29.4241,    -98.4936,   "USW00012921",
  "South",     "Dallas",             "TX",   32.7767,    -96.7970,   "USW00013960",
  "South",     "Fort Worth",         "TX",   32.7555,    -97.3308,   "USW00013961",
  "South",     "Jacksonville",       "FL",   30.3322,    -81.6557,   "USW00013889",
  "South",     "Austin",             "TX",   30.2672,    -97.7431,   "USW00013904",
  "South",     "Charlotte",          "NC",   35.2271,    -80.8431,   "USW00013881",
  "South",     "Nashville",          "TN",   36.1627,    -86.7816,   "USW00013897",
  "South",     "Oklahoma City",      "OK",   35.4676,    -97.5164,   "USW00013967",
  "South",     "Washington",         "DC",   38.9072,    -77.0369,   "USW00013743",
  "South",     "El Paso",            "TX",   31.7619,    -106.4850,  "USW00023044",
  "South",     "Memphis",            "TN",   35.1495,    -90.0490,   "USW00013893",
  
  # --- WEST REGION ---
  "West",      "Los Angeles",        "CA",   34.0522,    -118.2437,  "USW00023174",
  "West",      "Phoenix",            "AZ",   33.4484,    -112.0740,  "USW00023183",
  "West",      "San Diego",          "CA",   32.7157,    -117.1611,  "USW00023190",
  "West",      "San Jose",           "CA",   37.3382,    -121.8863,  "USW00023244",
  "West",      "San Francisco",      "CA",   37.7749,    -122.4194,  "USW00023272",
  "West",      "Seattle",            "WA",   47.6062,    -122.3321,  "USW00024233",
  "West",      "Denver",             "CO",   39.7392,    -104.9903,  "USW00023062",
  "West",      "Las Vegas",          "NV",   36.1699,    -115.1398,  "USW00023169",
  "West",      "Portland",           "OR",   45.5152,    -122.6784,  "USW00024229",
  "West",      "Fresno",             "CA",   36.7378,    -119.7871,  "USW00093193",
  "West",      "Albuquerque",        "NM",   35.0844,    -106.6504,  "USW00023050",
  "West",      "Tucson",             "AZ",   32.2226,    -110.9747,  "USW00023160"
)

message(sprintf("[Config Loaded] Successfully loaded %d cities across 4 U.S. regions.", nrow(city_coords)))
