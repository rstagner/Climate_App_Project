# ==============================================================================
# 1. SETUP & DATA LOADING
# ==============================================================================
library(tidyverse)

# Load joined daily climate dataset containing NOAA station and NASA POWER columns
climate_joined <- readRDS("data/climate_joined.rds")

# ==============================================================================
# 2. PLAIN-LANGUAGE EXPLANATION HELPER ("WHAT THIS MEANS")
# ==============================================================================
build_period_explanation <- function(city_label,
                                     var_label,
                                     source_label,
                                     a_span,
                                     b_span,
                                     a_mean,
                                     b_mean,
                                     diff_val,
                                     p_val_txt,
                                     p_num,
                                     test_label,
                                     n_a,
                                     n_b) {
  # Describe whether Period B is warmer, cooler, or unchanged relative to Period A
  direction_txt <- if (diff_val > 0) {
    paste0(sprintf("%+.2f", diff_val), " higher in Period B (", b_span, ") than Period A (", a_span, ")")
  } else if (diff_val < 0) {
    paste0(sprintf("%+.2f", diff_val), " lower in Period B (", b_span, ") than Period A (", a_span, ")")
  } else {
    "0.00 difference between Period A and Period B"
  }
  
  # Summarize statistical significance using the selected two-sample test
  sig_txt <- if (p_num < 0.05) {
    paste0(
      "Because p is ", p_val_txt, " (< 0.05), the difference between the two periods is statistically significant using a ",
      test_label, "."
    )
  } else {
    paste0(
      "Because p is ", p_val_txt, " (>= 0.05), the difference between the two periods is not statistically significant using a ",
      test_label, "."
    )
  }
  
  # Assemble a clear, student- and teacher-friendly takeaway paragraph
  paste0(
    city_label, " (", var_label, ", ", source_label, ") has a Period A (",
    a_span, ") mean of ", sprintf("%.2f", a_mean), " and a Period B (", b_span, ") mean of ",
    sprintf("%.2f", b_mean), "—a shift of ", direction_txt, ". ", sig_txt,
    " (Sample sizes: n = ", n_a, " daily records in Period A and n = ",
    n_b, " in Period B.)"
  )
}

# ==============================================================================
# 3. TWO-PERIOD COMPARISON FUNCTION
# ==============================================================================
run_period_comparison <- function(df,
                                  target_geoid = "92242",
                                  source_choice = c("NOAA weather station", "NASA POWER (satellite)"),
                                  var_choice = c("Mean temperature (°C)", "Max temperature (°C)", "Min temperature (°C)"),
                                  period_a_years = 1990:2005,
                                  period_b_years = 2011:2026) {
  # --- 3a. Validate inputs and resolve target column ---
  source_choice <- match.arg(source_choice)
  var_choice    <- match.arg(var_choice)
  
  col_name <- switch(
    paste(source_choice, var_choice, sep = " | "),
    "NOAA weather station | Mean temperature (°C)"   = "noaa_mean",
    "NOAA weather station | Max temperature (°C)"    = "noaa_max",
    "NOAA weather station | Min temperature (°C)"    = "noaa_min",
    "NASA POWER (satellite) | Mean temperature (°C)" = "nasa_mean",
    "NASA POWER (satellite) | Max temperature (°C)"  = "nasa_max",
    "NASA POWER (satellite) | Min temperature (°C)"  = "nasa_min"
  )
  
  stopifnot(col_name %in% names(df))
  
  # Match accent color to selected data source (NASA = burnt orange, NOAA = blue)
  line_color <- if (identical(source_choice, "NASA POWER (satellite)")) "#D95F02" else "#2166AC"
  
  # Normalize year boundaries so reversed inputs still work cleanly
  a_min <- min(as.integer(period_a_years), na.rm = TRUE)
  a_max <- max(as.integer(period_a_years), na.rm = TRUE)
  b_min <- min(as.integer(period_b_years), na.rm = TRUE)
  b_max <- max(as.integer(period_b_years), na.rm = TRUE)
  
  a_span <- paste0(a_min, "–", a_max)
  b_span <- paste0(b_min, "–", b_max)
  
  # --- 3b. Filter city & assign Period A / Period B rows ---
  city_df <- df |>
    mutate(
      GEOID      = as.character(GEOID),
      year       = as.integer(year),
      date       = as.Date(date),
      city_label = if ("state" %in% names(df)) paste0(city, ", ", state) else as.character(city)
    ) |>
    filter(
      GEOID == as.character(target_geoid),
      !is.na(.data[[col_name]]),
      !is.na(year)
    ) |>
    mutate(
      val = as.numeric(.data[[col_name]]),
      period = case_when(
        year >= a_min & year <= a_max ~ "Period A",
        year >= b_min & year <= b_max ~ "Period B",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(period))
  
  a_vals <- city_df |> filter(period == "Period A") |> pull(val)
  b_vals <- city_df |> filter(period == "Period B") |> pull(val)
  
  stopifnot(length(a_vals) >= 5, length(b_vals) >= 5)
  
  # --- 3c. Compute period means, difference (B - A), and hypothesis test ---
  a_mean   <- mean(a_vals, na.rm = TRUE)
  b_mean   <- mean(b_vals, na.rm = TRUE)
  diff_val <- b_mean - a_mean
  
  # Use Welch two-sample t-test when both periods have n >= 30, otherwise Wilcoxon rank-sum test
  use_welch <- (length(a_vals) >= 30L) && (length(b_vals) >= 30L)
  if (use_welch) {
    test_out   <- t.test(b_vals, a_vals, var.equal = FALSE)
    test_label <- "Welch two-sample t-test"
  } else {
    test_out   <- wilcox.test(b_vals, a_vals, exact = FALSE)
    test_label <- "Wilcoxon rank-sum test"
  }
  
  p_num     <- as.numeric(test_out$p.value)
  p_val_txt <- if (p_num < 0.001) "< 0.001" else sprintf("%.4f", p_num)
  
  cp_a_mean <- sprintf("%.2f", a_mean)
  cp_b_mean <- sprintf("%.2f", b_mean)
  cp_diff   <- sprintf("%+.2f", diff_val)
  
  city_lbl  <- unique(city_df$city_label)[1]
  p_a_label <- paste0("Period A (", a_span, ")")
  p_b_label <- paste0("Period B (", b_span, ")")
  
  # Build plain-language interpretation text
  cp_text <- build_period_explanation(
    city_label   = city_lbl,
    var_label    = var_choice,
    source_label = source_choice,
    a_span       = a_span,
    b_span       = b_span,
    a_mean       = a_mean,
    b_mean       = b_mean,
    diff_val     = diff_val,
    p_val_txt    = p_val_txt,
    p_num        = p_num,
    test_label   = test_label,
    n_a          = length(a_vals),
    n_b          = length(b_vals)
  )
  
  # --- 3d. Build distribution plot (violin "smoke" + boxplot + mean marker) ---
  plot_df <- city_df |>
    mutate(
      period_lbl = factor(
        ifelse(period == "Period A", p_a_label, p_b_label),
        levels = c(p_a_label, p_b_label)
      )
    )
  
  p_smoke_box <- ggplot(plot_df, aes(x = period_lbl, y = val, fill = period_lbl)) +
    geom_violin(alpha = 0.25, trim = TRUE, color = NA) +                       # Bounded density ("smoke") layer
    geom_boxplot(width = 0.22, outlier.alpha = 0.2, color = "#111111") +      # Median, IQR, and outliers
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3.2, color = line_color) + # Mean diamond
    scale_fill_manual(values = setNames(c("#2166AC", "#D95F02"), c(p_a_label, p_b_label))) +
    labs(
      title = paste0(city_lbl, " — ", var_choice, " by Period"),
      subtitle = paste0(
        source_choice, " | ", test_label,
        " | Diff (B - A): ", cp_diff,
        " | p = ", p_val_txt
      ),
      x = NULL,
      y = var_choice
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none")
  
  # Return formatted outputs matching Shiny cp_* fields
  list(
    cp_a_mean  = cp_a_mean,
    cp_b_mean  = cp_b_mean,
    cp_diff    = cp_diff,
    cp_p       = p_val_txt,
    cp_text    = cp_text,
    test_used  = test_label,
    plot       = p_smoke_box
  )
}

# ==============================================================================
# 3. CORE TWO-PERIOD COMPARISON FUNCTION
# ==============================================================================
run_period_comparison <- function(df,
                                  target_geoid = "92242",
                                  source_choice = c("NOAA weather station", "NASA POWER (satellite)"),
                                  var_choice = c("Mean temperature (°C)", "Max temperature (°C)", "Min temperature (°C)"),
                                  period_a_years = 1990:2005,
                                  period_b_years = 2011:2026) {
  # --- 3a. Validate inputs and resolve target column ---
  source_choice <- match.arg(source_choice)
  var_choice    <- match.arg(var_choice)

  col_name <- switch(
    paste(source_choice, var_choice, sep = " | "),
    "NOAA weather station | Mean temperature (°C)"   = "noaa_mean",
    "NOAA weather station | Max temperature (°C)"    = "noaa_max",
    "NOAA weather station | Min temperature (°C)"    = "noaa_min",
    "NASA POWER (satellite) | Mean temperature (°C)" = "nasa_mean",
    "NASA POWER (satellite) | Max temperature (°C)"  = "nasa_max",
    "NASA POWER (satellite) | Min temperature (°C)"  = "nasa_min"
  )

  stopifnot(col_name %in% names(df))

  # Match accent color to selected data source (NASA = burnt orange, NOAA = blue)
  line_color <- if (identical(source_choice, "NASA POWER (satellite)")) "#D95F02" else "#2166AC"

  # Normalize year boundaries so reversed inputs still work cleanly
  a_min <- min(as.integer(period_a_years), na.rm = TRUE)
  a_max <- max(as.integer(period_a_years), na.rm = TRUE)
  b_min <- min(as.integer(period_b_years), na.rm = TRUE)
  b_max <- max(as.integer(period_b_years), na.rm = TRUE)

  a_span <- paste0(a_min, "–", a_max)
  b_span <- paste0(b_min, "–", b_max)

  # --- 3b. Filter city & assign Period A / Period B rows ---
  city_df <- df |>
    mutate(
      GEOID      = as.character(GEOID),
      year       = as.integer(year),
      date       = as.Date(date),
      city_label = if ("state" %in% names(df)) paste0(city, ", ", state) else as.character(city)
    ) |>
    filter(
      GEOID == as.character(target_geoid),
      !is.na(.data[[col_name]]),
      !is.na(year)
    ) |>
    mutate(
      val = as.numeric(.data[[col_name]]),
      period = case_when(
        year >= a_min & year <= a_max ~ "Period A",
        year >= b_min & year <= b_max ~ "Period B",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(period))

  a_vals <- city_df |> filter(period == "Period A") |> pull(val)
  b_vals <- city_df |> filter(period == "Period B") |> pull(val)

  stopifnot(length(a_vals) >= 5, length(b_vals) >= 5)

  # --- 3c. Compute period means, difference (B - A), and hypothesis test ---
  a_mean   <- mean(a_vals, na.rm = TRUE)
  b_mean   <- mean(b_vals, na.rm = TRUE)
  diff_val <- b_mean - a_mean

  # Use Welch two-sample t-test when both periods have n >= 30, otherwise Wilcoxon rank-sum test
  use_welch <- (length(a_vals) >= 30L) && (length(b_vals) >= 30L)
  if (use_welch) {
    test_out   <- t.test(b_vals, a_vals, var.equal = FALSE)
    test_label <- "Welch two-sample t-test"
  } else {
    test_out   <- wilcox.test(b_vals, a_vals, exact = FALSE)
    test_label <- "Wilcoxon rank-sum test"
  }

  p_num     <- as.numeric(test_out$p.value)
  p_val_txt <- if (p_num < 0.001) "< 0.001" else sprintf("%.4f", p_num)

  cp_a_mean <- sprintf("%.2f", a_mean)
  cp_b_mean <- sprintf("%.2f", b_mean)
  cp_diff   <- sprintf("%+.2f", diff_val)

  city_lbl  <- unique(city_df$city_label)[1]
  p_a_label <- paste0("Period A (", a_span, ")")
  p_b_label <- paste0("Period B (", b_span, ")")

  # Build plain-language interpretation text
  cp_text <- build_period_explanation(
    city_label   = city_lbl,
    var_label    = var_choice,
    source_label = source_choice,
    a_span       = a_span,
    b_span       = b_span,
    a_mean       = a_mean,
    b_mean       = b_mean,
    diff_val     = diff_val,
    p_val_txt    = p_val_txt,
    p_num        = p_num,
    test_label   = test_label,
    n_a          = length(a_vals),
    n_b          = length(b_vals)
  )

  # --- 3d. Build distribution plot (violin "smoke" + boxplot + mean marker) ---
  plot_df <- city_df |>
    mutate(
      period_lbl = factor(
        ifelse(period == "Period A", p_a_label, p_b_label),
        levels = c(p_a_label, p_b_label)
      )
    )

  p_smoke_box <- ggplot(plot_df, aes(x = period_lbl, y = val, fill = period_lbl)) +
    geom_violin(alpha = 0.25, trim = TRUE, color = NA) +                       # Bounded density ("smoke") layer
    geom_boxplot(width = 0.22, outlier.alpha = 0.2, color = "#111111") +      # Median, IQR, and outliers
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3.2, color = line_color) + # Mean diamond
    scale_fill_manual(values = setNames(c("#2166AC", "#D95F02"), c(p_a_label, p_b_label))) +
    labs(
      title = paste0(city_lbl, " — ", var_choice, " by Period"),
      subtitle = paste0(
        source_choice, " | ", test_label,
        " | Diff (B - A): ", cp_diff,
        " | p = ", p_val_txt
      ),
      x = NULL,
      y = var_choice
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none")

  # Return formatted outputs matching Shiny cp_* fields
  list(
    cp_a_mean  = cp_a_mean,
    cp_b_mean  = cp_b_mean,
    cp_diff    = cp_diff,
    cp_p       = p_val_txt,
    cp_text    = cp_text,
    test_used  = test_label,
    plot       = p_smoke_box
  )
}

# ==============================================================================
# 4. stat values, plots,  and plain language print
# ==============================================================================
out <- run_period_comparison(climate_joined)

# Print summary metrics and test used
print(out[c("cp_a_mean", "cp_b_mean", "cp_diff", "cp_p", "test_used")])

# Print plain-language explanation ("What this means")
cat("\nWhat this means:\n", out$cp_text, "\n", sep = "")

# Render distribution plot
print(out$plot)
