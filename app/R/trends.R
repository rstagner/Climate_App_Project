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

# prepare annual data for a single city, source, and variable
prepare_trends_data <- function(annual,
                                city_id,
                                source_choice,
                                var_label,
                                year_from = 1990,
                                year_to = 2026,
                                min_days = 330L) {
  shiny::validate(
    shiny::need(!is.null(city_id) && length(city_id) == 1 && nzchar(city_id),
                "Please choose a city.")
  )
  
  var_meta <- map_trends_var(var_label)
  yr_min   <- min(as.integer(year_from), as.integer(year_to), na.rm = TRUE)
  yr_max   <- max(as.integer(year_from), as.integer(year_to), na.rm = TRUE)
  
  use_noaa <- identical(source_choice, "NOAA weather station")
  col_name <- if (use_noaa) var_meta$noaa_col else var_meta$nasa_col
  day_col  <- if (use_noaa) "noaa_days" else "nasa_days"
  
  shiny::validate(
    shiny::need(col_name %in% names(annual),
                paste0("Column '", col_name, "' not found in climate_annual."))
  )
  
  df <- annual |>
    dplyr::mutate(
      GEOID      = as.character(GEOID),
      year       = as.integer(year),
      city_label = if ("CITY" %in% names(annual) && "STATE" %in% names(annual)) {
        paste0(CITY, ", ", STATE)
      } else if ("city" %in% names(annual) && "state" %in% names(annual)) {
        paste0(city, ", ", state)
      } else {
        as.character(GEOID)
      },
      value = as.numeric(.data[[col_name]])
    ) |>
    dplyr::filter(
      GEOID == as.character(city_id),
      year >= yr_min,
      year <= yr_max
    )
  
  if (day_col %in% names(df)) {
    df <- df |>
      dplyr::filter(!is.na(.data[[day_col]]), .data[[day_col]] >= min_days)
  }
  
  df <- df |>
    dplyr::filter(!is.na(value)) |>
    dplyr::arrange(year)
  
  shiny::validate(
    shiny::need(nrow(df) >= 5,
                "Not enough complete years (>= 330 days) to fit a reliable trend.")
  )
  
  df
}

# fit linear regression over time and return metrics in °C per decade
fit_trends_model <- function(trend_df) {
  fit <- stats::lm(value ~ year, data = trend_df)
  sm  <- summary(fit)
  coef_tbl <- sm$coefficients
  
  # convert annual slope (°C/year) to °C/decade
  slope_yr  <- coef_tbl["year", "Estimate"]
  se_yr     <- coef_tbl["year", "Std. Error"]
  p_val     <- coef_tbl["year", "Pr(>|t|)"]
  r2_val    <- sm$r.squared
  
  slope_dec <- slope_yr * 10
  ci_raw    <- stats::confint(fit, "year", level = 0.95) * 10
  
  list(
    model      = fit,
    data       = trend_df |>
      dplyr::mutate(
        fitted   = stats::fitted(fit),
        resid    = stats::residuals(fit),
        std_resid = stats::rstandard(fit)
      ),
    city_label = trend_df$city_label[1],
    year_start = min(trend_df$year),
    year_end   = max(trend_df$year),
    n_years    = nrow(trend_df),
    slope_dec  = slope_dec,
    ci_low     = ci_raw[1],
    ci_high    = ci_raw[2],
    p_value    = p_val,
    r_squared  = r2_val
  )
}
