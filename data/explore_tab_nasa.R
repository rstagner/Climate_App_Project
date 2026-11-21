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

# =========================================================================
# 2. LOAD & PREPARE DATA (ALL CITIES)
# =========================================================================
if (!file.exists(data_path)) {
  stop(sprintf("File not found: '%s'. Run your NASA tidy script first.", data_path))
}

nasa_tidy <- readRDS(data_path) |>
  mutate(
    date       = as.Date(date),
    city_label = paste0(city, ", ", state)
  )

if (!target_col %in% names(nasa_tidy)) {
  stop(sprintf("Column '%s' not found in %s.", target_col, data_path))
}

if (is.null(end_date)) {
  end_date <- nasa_tidy |>
    filter(!is.na(.data[[target_col]])) |>
    summarise(max_dt = max(date, na.rm = TRUE)) |>
    pull(max_dt)
}

nasa_filtered <- nasa_tidy |>
  filter(date >= start_date, date <= end_date) |>
  mutate(selected_temp = .data[[target_col]])
