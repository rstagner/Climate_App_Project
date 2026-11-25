# Helper functions for Statistics -> Compare Two Periods (cp_*)

# map Period Comparison UI variable choices to climate_annual columns
map_cp_var <- function(var_label) {
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

# filter and label two non-overlapping (or distinct) year periods for one city
prepare_cp_data <- function(annual,
                            city_id,
                            source_choice,
                            var_label,
                            a_from = 1990,
                            a_to = 2005,
                            b_from = 2010,
                            b_to = 2026,
                            min_days = 330L) {
  shiny::validate(
    shiny::need(!is.null(city_id) && length(city_id) == 1 && nzchar(city_id),
                "Please choose a city.")
  )
  
  a_min <- min(as.integer(a_from), as.integer(a_to), na.rm = TRUE)
  a_max <- max(as.integer(a_from), as.integer(a_to), na.rm = TRUE)
  b_min <- min(as.integer(b_from), as.integer(b_to), na.rm = TRUE)
  b_max <- max(as.integer(b_from), as.integer(b_to), na.rm = TRUE)
  
  shiny::validate(
    shiny::need(a_max < b_min || b_max < a_min,
                "Please choose two non-overlapping year ranges for Period A and Period B.")
  )
  
  var_meta <- map_cp_var(var_label)
  use_noaa <- identical(source_choice, "NOAA weather station")
  col_name <- if (use_noaa) var_meta$noaa_col else var_meta$nasa_col
  day_col  <- if (use_noaa) "noaa_days" else "nasa_days"
  
  shiny::validate(
    shiny::need(col_name %in% names(annual),
                paste0("Column '", col_name, "' not found in climate_annual."))
  )
  
  base_df <- annual |>
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
    dplyr::filter(GEOID == as.character(city_id))
  
  if (day_col %in% names(base_df)) {
    base_df <- base_df |>
      dplyr::filter(!is.na(.data[[day_col]]), .data[[day_col]] >= min_days)
  }
  
  period_a_label <- paste0("Period A (", a_min, "–", a_max, ")")
  period_b_label <- paste0("Period B (", b_min, "–", b_max, ")")
  
  df_a <- base_df |>
    dplyr::filter(year >= a_min, year <= a_max, !is.na(value)) |>
    dplyr::mutate(period = period_a_label, period_role = "Period A")
  
  df_b <- base_df |>
    dplyr::filter(year >= b_min, year <= b_max, !is.na(value)) |>
    dplyr::mutate(period = period_b_label, period_role = "Period B")
  
  shiny::validate(
    shiny::need(nrow(df_a) >= 3 && nrow(df_b) >= 3,
                "Each period needs at least 3 complete years (>= 330 days) to compare.")
  )
  
  dplyr::bind_rows(df_a, df_b) |>
    dplyr::mutate(
      period = factor(period, levels = c(period_a_label, period_b_label))
    )
}