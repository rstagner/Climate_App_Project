library(tidyverse)

# ------------------------------------------------------------------------------
# Load annual climate data and city lookup
# ------------------------------------------------------------------------------
climate_annual <- readRDS("data/climate_annual.rds")
city_stations  <- readRDS("data/city_stations.rds")

if (all(c("CITY", "STATE", "GEOID") %in% names(city_stations))) {
  city_list <- city_stations |>
    mutate(label = paste0(CITY, ", ", STATE)) |>
    arrange(label)
  city_choices <- setNames(as.character(city_list$GEOID), city_list$label)
} else {
  city_choices <- NULL
}

# ------------------------------------------------------------------------------
# Test parameters (one city, one source, one temperature variable)
# ------------------------------------------------------------------------------
selected_city     <- "92242"                 # Washington, DC default GEOID
selected_source   <- "NOAA weather station"    # "NOAA weather station" or "NASA POWER (satellite)"
selected_variable <- "Mean temperature (°C)" # "Mean temperature (°C)", "Max temperature (°C)", "Min temperature (°C)"
yr_from           <- 1990L
yr_to             <- 2026L
min_annual_days   <- 330L                    # Require nearly full year coverage (data runs through Oct 31, 2026)

source_prefix <- if (identical(selected_source, "NASA POWER (satellite)")) "nasa" else "noaa"
days_col      <- paste0(source_prefix, "_days")
line_color    <- if (identical(selected_source, "NASA POWER (satellite)")) "#D95F02" else "#2166AC"

temp_col <- switch(
  selected_variable,
  "Mean temperature (°C)" = paste0(source_prefix, "_mean"),
  "Max temperature (°C)"  = paste0(source_prefix, "_max"),
  "Min temperature (°C)"  = paste0(source_prefix, "_min"),
  stop("Unsupported variable for Trends: ", selected_variable)
)

cat("=== Trends Console Test ===\n")
cat("City GEOID: ", selected_city, "\n", sep = "")
cat("Source:     ", selected_source, " (", line_color, ")\n", sep = "")
cat("Variable:   ", selected_variable, " -> ", temp_col, "\n", sep = "")
cat("Window:     ", yr_from, "–", yr_to, " (data through Oct 31, 2026)\n\n", sep = "")

# ------------------------------------------------------------------------------
# Filter to city, year range, and annual completeness
# ------------------------------------------------------------------------------
city_df <- climate_annual |>
  mutate(
    GEOID = as.character(GEOID),
    year  = as.integer(year),
    city_label = if ("state" %in% names(climate_annual)) {
      paste0(city, ", ", state)
    } else {
      as.character(city)
    }
  ) |>
  filter(
    GEOID == as.character(selected_city),
    year >= yr_from,
    year <= yr_to
  )

stopifnot(nrow(city_df) > 0)
city_label_use <- unique(city_df$city_label)[1]

if (days_col %in% names(city_df)) {
  excluded_years <- city_df |>
    filter(is.na(.data[[days_col]]) | .data[[days_col]] < min_annual_days) |>
    pull(year)
  
  cat("Excluded years (", days_col, " < ", min_annual_days, "): ",
      if (length(excluded_years) > 0) paste(excluded_years, collapse = ", ") else "none",
      "\n", sep = "")
  
  model_df <- city_df |>
    filter(!is.na(.data[[days_col]]), .data[[days_col]] >= min_annual_days)
} else {
  cat("Note: ", days_col, " missing; skipping day-count filter.\n", sep = "")
  model_df <- city_df
}

model_df <- model_df |>
  filter(!is.na(.data[[temp_col]]), !is.na(year)) |>
  mutate(temp_c = as.numeric(.data[[temp_col]])) |>
  arrange(year)

cat("Complete years in model: ", nrow(model_df),
    " (", min(model_df$year), "–", max(model_df$year), ")\n\n", sep = "")
stopifnot(nrow(model_df) >= 3)

