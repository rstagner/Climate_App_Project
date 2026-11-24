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

