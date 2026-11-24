# ==============================================================================
# 1. SETUP & DATA LOADING
# ==============================================================================
library(tidyverse)

# Load joined daily climate dataset containing NOAA station and NASA POWER columns
climate_joined <- readRDS("data/climate_joined.rds")
