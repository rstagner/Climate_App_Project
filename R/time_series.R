
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

# -------------------------------------------------------------------------
# 2. Helper: Filter and reshape climate_joined for temperature plotting
# -------------------------------------------------------------------------
prep_explore_ts_data <- function(
    df,
    cities,
    source = c("NASA POWER (satellite)", "NOAA weather station", "Both, overlaid"),
    variable = "Mean temperature (°C)",
    year_from = 1990,
    year_to = 2026
) {
  source <- match.arg(source)
  
  if (length(cities) == 0) {
    stop("At least one city GEOID must be selected.")
  }
  if (length(cities) > 4) {
    cities <- cities[1:4]
  }
  
  cities_chr <- as.character(cities)
  yr_min <- min(as.integer(year_from), as.integer(year_to))
  yr_max <- max(as.integer(year_from), as.integer(year_to))
  cols <- get_temp_cols(variable)
  
  base_df <- df |>
    mutate(
      GEOID = as.character(GEOID),
      date = as.Date(date),
      year = as.integer(year),
      city_label = paste0(city, ", ", state)
    ) |>
    filter(
      GEOID %in% cities_chr,
      year >= yr_min,
      year <= yr_max
    )
  
  nasa_tbl <- base_df |>
    filter(!is.na(.data[[cols$nasa]])) |>
    transmute(
      GEOID,
      city_label,
      date,
      year,
      source_label = "NASA POWER (satellite)",
      value = as.numeric(.data[[cols$nasa]])
    )
  
  noaa_tbl <- base_df |>
    filter(!is.na(.data[[cols$noaa]])) |>
    transmute(
      GEOID,
      city_label,
      date,
      year,
      source_label = "NOAA weather station",
      value = as.numeric(.data[[cols$noaa]])
    )
  
  switch(
    source,
    "NASA POWER (satellite)" = nasa_tbl,
    "NOAA weather station"   = noaa_tbl,
    "Both, overlaid"         = bind_rows(nasa_tbl, noaa_tbl)
  )
}

# -------------------------------------------------------------------------
# 3. Pure R Plot Function (returns a ggplot object)
# -------------------------------------------------------------------------
plot_explore_timeseries <- function(
    df,
    cities,
    source = c("NASA POWER (satellite)", "NOAA weather station", "Both, overlaid"),
    variable = "Mean temperature (°C)",
    year_from = 1990,
    year_to = 2026
) {
  source <- match.arg(source)
  
  plot_df <- prep_explore_ts_data(
    df = df,
    cities = cities,
    source = source,
    variable = variable,
    year_from = year_from,
    year_to = year_to
  )
  
  if (nrow(plot_df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No non-missing temperature data for this selection.") +
        theme_void()
    )
  }
  
  if (identical(source, "Both, overlaid")) {
    p <- ggplot(
      plot_df,
      aes(x = date, y = value, color = source_label, linetype = source_label)
    ) +
      geom_line(linewidth = 0.45, alpha = 0.85, na.rm = TRUE) +
      facet_wrap(~ city_label, scales = "free_y") +
      labs(
        title = paste0(variable, " — ", source),
        subtitle = paste0("Years: ", min(plot_df$year), "–", max(plot_df$year)),
        x = "Date",
        y = variable,
        color = "Source",
        linetype = "Source"
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top")
  } else {
    p <- ggplot(
      plot_df,
      aes(x = date, y = value, color = city_label)
    ) +
      geom_line(linewidth = 0.45, alpha = 0.85, na.rm = TRUE) +
      labs(
        title = paste0(variable, " — ", source),
        subtitle = paste0("Years: ", min(plot_df$year), "–", max(plot_df$year)),
        x = "Date",
        y = variable,
        color = "City"
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top")
  }
  
  p
}

# -------------------------------------------------------------------------
# 4. Standalone Test Blocks
# -------------------------------------------------------------------------

test_geoids <- if ("92242" %in% climate_joined$GEOID) {
  c("92242", "00970")
} else {
  unique(climate_joined$GEOID)[1:2]
}

# Test 1: NASA only
print(
  plot_explore_timeseries(
    df = climate_joined,
    cities = test_geoids,
    source = "NASA POWER (satellite)",
    variable = "Mean temperature (°C)",
    year_from = 2020,
    year_to = 2026
  )
)

# Test 2: NOAA only
print(
  plot_explore_timeseries(
    df = climate_joined,
    cities = test_geoids,
    source = "NOAA weather station",
    variable = "Max temperature (°C)",
    year_from = 2020,
    year_to = 2026
  )
)

# Test 3: Both overlaid
print(
  plot_explore_timeseries(
    df = climate_joined,
    cities = test_geoids,
    source = "Both, overlaid",
    variable = "Min temperature (°C)",
    year_from = 2022,
    year_to = 2026
  )
)
