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

# run two-sample comparison (Welch's t-test + Wilcoxon rank-sum reference)
run_cp_test <- function(cp_df) {
  vals_a <- cp_df$value[cp_df$period_role == "Period A"]
  vals_b <- cp_df$value[cp_df$period_role == "Period B"]
  
  mean_a <- mean(vals_a, na.rm = TRUE)
  mean_b <- mean(vals_b, na.rm = TRUE)
  diff_ba <- mean_b - mean_a
  
  t_res <- stats::t.test(vals_b, vals_a, var.equal = FALSE)
  w_res <- stats::wilcox.test(vals_b, vals_a, exact = FALSE)
  
  list(
    data           = cp_df,
    city_label     = cp_df$city_label[1],
    period_a_label = levels(cp_df$period)[1],
    period_b_label = levels(cp_df$period)[2],
    n_a            = length(vals_a),
    n_b            = length(vals_b),
    mean_a         = mean_a,
    mean_b         = mean_b,
    diff_ba        = diff_ba,
    ci_low         = t_res$conf.int[1],
    ci_high        = t_res$conf.int[2],
    p_value        = t_res$p.value,
    wilcox_p       = w_res$p.value
  )
}

# plot distribution (boxplot + jittered yearly points) for Period A vs Period B
plot_cp_dist <- function(cp_res, var_label) {
  var_meta <- map_cp_var(var_label)
  df       <- cp_res$data
  
  ggplot2::ggplot(df, ggplot2::aes(x = period, y = value, fill = period_role)) +
    ggplot2::geom_boxplot(width = 0.45, alpha = 0.35, outlier.shape = NA) +
    ggplot2::geom_jitter(
      ggplot2::aes(color = period_role),
      width = 0.12, height = 0, size = 2.2, alpha = 0.85
    ) +
    ggplot2::scale_fill_manual(
      values = c("Period A" = "#1f5a8a", "Period B" = "#d95f02"),
      guide  = "none"
    ) +
    ggplot2::scale_color_manual(
      values = c("Period A" = "#1f5a8a", "Period B" = "#d95f02"),
      guide  = "none"
    ) +
    ggplot2::labs(
      title    = paste0(cp_res$city_label, ": ", cp_res$period_b_label, " vs. ", cp_res$period_a_label),
      subtitle = sprintf("Mean difference (B - A): %+.2f °C (Welch p = %s)",
                         cp_res$diff_ba,
                         if (cp_res$p_value < 0.001) "< 0.001" else sprintf("%.3f", cp_res$p_value)),
      x        = NULL,
      y        = var_meta$y_label
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}
