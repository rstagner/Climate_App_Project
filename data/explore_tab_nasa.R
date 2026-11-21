library(dplyr)
library(tidyr)
library(ggplot2)

# =========================================================================
# 1. PARAMETERS
# =========================================================================
data_path <- "data/nasa_power_tidy.rds"

# Choose which temperature variable to highlight in the boxplot & summary:
# "nasa_mean", "nasa_max", or "nasa_min"
target_col <- "nasa_mean"

# Optional date filter (leave end_date NULL to stop at the last date with
# non-missing values and avoid the recent tail NAs from 2026-11-17 onward)
start_date <- as.Date("1990-01-01")
end_date   <- NULL
