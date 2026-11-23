
# -------------------------------------------------------------------------
# 0. Load CLimate Joined and Inspect
# -------------------------------------------------------------------------

climate_joined <- readRDS("data/climate_joined.rds")

# Print exact column names
names(climate_joined)

# inspect column types and first few rows
dplyr::glimpse(climate_joined)

library(dplyr)
library(ggplot2)

# -------------------------------------------------------------------------
# 1. Helper: Map UI source + temperature variable to exact column(s)
# -------------------------------------------------------------------------
get_temp_cols <- function(variable_label) {
  switch(
    variable_label,
    "Mean temperature (°C)" = list(nasa = "nasa_mean", noaa = "noaa_mean"),
    "Max temperature (°C)"  = list(nasa = "nasa_max",  noaa = "noaa_max"),
    "Min temperature (°C)"  = list(nasa = "nasa_min",  noaa = "noaa_min"),
    stop("Unsupported temperature variable: ", variable_label)
  )
}
