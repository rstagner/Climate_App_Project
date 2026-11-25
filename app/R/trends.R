# Helper functions for Statistics -> Trends (tr_*)

# map Trends UI variable choices to climate_annual columns
map_trends_var <- function(var_label) {
  switch(
    var_label,
    "Mean temperature (°C)" = list(
      noaa_col = "noaa_mean",
      nasa_col = "nasa_mean",
      y_label  = "Mean temperature (°C)"
    ),
    "Max temperature (°C)" = list(
      noaa_col = "noaa_max",
      nasa_col = "nasa_max",
      y_label  = "Max temperature (°C)"
    ),
    "Min temperature (°C)" = list(
      noaa_col = "noaa_min",
      nasa_col = "nasa_min",
      y_label  = "Min temperature (°C)"
    ),
    list(
      noaa_col = "noaa_mean",
      nasa_col = "nasa_mean",
      y_label  = "Mean temperature (°C)"
    )
  )
}

