# Helper functions for Explore -> Time Series (ts_plot)

# map Explore UI variable choices to climate_annual columns
map_explore_ts_var <- function(var_label) {
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
    "Precipitation (mm)" = list(
      noaa_col = "noaa_precip",
      nasa_col = "nasa_precip",
      y_label  = "Precipitation (mm)"
    ),
    list(
      noaa_col = "noaa_mean",
      nasa_col = "nasa_mean",
      y_label  = "Mean temperature (°C)"
    )
  )
}

# filter by city, source, and year range, keeping years with >= 330 days
prepare_explore_ts_data <- function(annual,
                                    city_ids,
                                    source_choice,
                                    var_label,
                                    year_from = 1990,
                                    year_to = 2026,
                                    min_days = 330L) {
  shiny::validate(
    shiny::need(length(city_ids) > 0, "Please choose at least one city.")
  )
  
  var_meta <- map_explore_ts_var(var_label)
  yr_min   <- min(as.integer(year_from), as.integer(year_to), na.rm = TRUE)
  yr_max   <- max(as.integer(year_from), as.integer(year_to), na.rm = TRUE)
  
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
      }
    ) |>
    dplyr::filter(
      GEOID %in% as.character(city_ids),
      year >= yr_min,
      year <= yr_max
    )
  
  shiny::validate(
    shiny::need(nrow(base_df) > 0, "No records match the selected cities and year range.")
  )
  
  series_list <- list()
  
  # pull NOAA station series
  if (source_choice %in% c("NOAA weather station", "Both, overlaid")) {
    shiny::validate(
      shiny::need(var_meta$noaa_col %in% names(base_df),
                  paste0("Column '", var_meta$noaa_col, "' not found in climate_annual."))
    )
    noaa_sub <- base_df
    if ("noaa_days" %in% names(noaa_sub)) {
      noaa_sub <- noaa_sub |>
        dplyr::filter(!is.na(noaa_days), noaa_days >= min_days)
    }
    series_list[["noaa"]] <- noaa_sub |>
      dplyr::transmute(
        GEOID,
        city_label,
        year,
        source = "NOAA weather station",
        value  = as.numeric(.data[[var_meta$noaa_col]])
      ) |>
      dplyr::filter(!is.na(value))
  }
  
  # pull NASA POWER series
  if (source_choice %in% c("NASA POWER (satellite)", "Both, overlaid")) {
    shiny::validate(
      shiny::need(var_meta$nasa_col %in% names(base_df),
                  paste0("Column '", var_meta$nasa_col, "' not found in climate_annual."))
    )
    nasa_sub <- base_df
    if ("nasa_days" %in% names(nasa_sub)) {
      nasa_sub <- nasa_sub |>
        dplyr::filter(!is.na(nasa_days), nasa_days >= min_days)
    }
    series_list[["nasa"]] <- nasa_sub |>
      dplyr::transmute(
        GEOID,
        city_label,
        year,
        source = "NASA POWER (satellite)",
        value  = as.numeric(.data[[var_meta$nasa_col]])
      ) |>
      dplyr::filter(!is.na(value))
  }
  
  out <- dplyr::bind_rows(series_list) |>
    dplyr::arrange(city_label, source, year)
  
  shiny::validate(
    shiny::need(nrow(out) > 0, "No complete years (>= 330 days) available for this selection.")
  )
  
  out
}

# plot annual time series, faceting when multiple cities are selected
plot_explore_ts <- function(ts_df, var_label) {
  var_meta <- map_explore_ts_var(var_label)
  n_cities <- dplyr::n_distinct(ts_df$city_label)
  
  source_palette <- c(
    "NOAA weather station"   = "#1f5a8a",
    "NASA POWER (satellite)" = "#d95f02"
  )
  
  p <- ggplot2::ggplot(ts_df, ggplot2::aes(x = year, y = value, color = source)) +
    ggplot2::geom_line(linewidth = 0.85, alpha = 0.9) +
    ggplot2::geom_point(size = 1.6, alpha = 0.85) +
    ggplot2::scale_color_manual(values = source_palette, name = "Data source") +
    ggplot2::labs(
      title = var_label,
      x     = "Year",
      y     = var_meta$y_label
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position  = "top",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  if (n_cities > 1) {
    p <- p + ggplot2::facet_wrap(~ city_label, scales = "free_y")
  } else {
    p <- p + ggplot2::labs(subtitle = unique(ts_df$city_label)[1])
  }
  
  p
}
