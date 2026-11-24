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
# ------------------------------------------------------------------------------
# Fit linear trend and scale slope + 95% CI to °C per decade
# ------------------------------------------------------------------------------
tr_model   <- lm(temp_c ~ year, data = model_df)
tr_summary <- summary(tr_model)
tr_coef    <- coef(tr_summary)

slope_yr     <- tr_coef["year", "Estimate"]
p_val        <- tr_coef["year", "Pr(>|t|)"]
r2_val       <- tr_summary$r.squared
ci_yr        <- confint(tr_model, "year", level = 0.95)

# Multiply per-year estimates by 10 so both slope and CI are in °C per decade
slope_decade <- slope_yr * 10
ci_lo        <- ci_yr[1] * 10
ci_hi        <- ci_yr[2] * 10

stopifnot(all(is.finite(c(slope_decade, ci_lo, ci_hi, p_val, r2_val))))

tr_slope <- sprintf("%+.2f", slope_decade)
tr_ci    <- sprintf("[%+.2f, %+.2f]", ci_lo, ci_hi)
tr_p     <- if (p_val < 0.001) "< 0.001" else sprintf("%.4f", p_val)
tr_r2    <- sprintf("%.3f", r2_val)

cat("--- Stat cards ---\n")
cat("tr_slope (°C/decade): ", tr_slope, "\n", sep = "")
cat("tr_ci (95% CI):       ", tr_ci, "\n", sep = "")
cat("tr_p (p-value):       ", tr_p, "\n", sep = "")
cat("tr_r2 (R²):           ", tr_r2, "\n\n", sep = "")

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

# ------------------------------------------------------------------------------
# Model assumption & diagnostic checks
# ------------------------------------------------------------------------------
res_vals   <- residuals(tr_model)
fit_vals   <- fitted(tr_model)
cooks_vals <- cooks.distance(tr_model)

# Check lag-1 autocorrelation in residuals (values near 2 indicate little serial correlation)
if (requireNamespace("lmtest", quietly = TRUE)) {
  dw_test_res <- lmtest::dwtest(tr_model)
  dw_stat     <- as.numeric(dw_test_res$statistic)
  dw_p        <- as.numeric(dw_test_res$p.value)
  dw_p_txt    <- if (dw_p < 0.001) "< 0.001" else sprintf("%.3f", dw_p)
  dw_metric   <- sprintf("DW = %.2f, p = %s", dw_stat, dw_p_txt)
} else {
  dw_stat   <- sum(diff(res_vals)^2) / sum(res_vals^2)
  dw_metric <- sprintf("DW = %.2f", dw_stat)
}

# Check linear trend in residual spread across fitted values
abs_resid_cor <- suppressWarnings(cor(abs(res_vals), fit_vals, use = "complete.obs"))
abs_cor_txt   <- sprintf("%.2f", abs(abs_resid_cor))

# Check residual normality alongside the Q-Q plot
shapiro_out <- if (length(res_vals) >= 3 && length(res_vals) <= 5000) {
  shapiro.test(res_vals)
} else {
  NULL
}
shapiro_p_txt <- if (!is.null(shapiro_out)) {
  if (shapiro_out$p.value < 0.001) "< 0.001" else sprintf("%.4f", shapiro_out$p.value)
} else {
  "N/A"
}

top_cooks_idx <- which.max(cooks_vals)

cat("--- Diagnostics ---\n")
cat("Independence (", dw_metric, ")\n", sep = "")
cat("Spread vs. fitted |r|:        ", abs_cor_txt,
    " (sigma = ", sprintf("%.3f °C", tr_summary$sigma), ")\n", sep = "")
if (!is.null(shapiro_out)) {
  cat("Shapiro-Wilk:                 W = ", sprintf("%.4f", shapiro_out$statistic),
      ", p = ", shapiro_p_txt, "\n", sep = "")
}
cat("Max Cook's D:                 ", sprintf("%.3f", cooks_vals[top_cooks_idx]),
    " (", model_df$year[top_cooks_idx], ")\n\n", sep = "")

# ------------------------------------------------------------------------------
# Plain-language summary for students and teachers (tr_text)
# ------------------------------------------------------------------------------
ci_note <- if (ci_lo > 0) {
  "stays above zero"
} else if (ci_hi < 0) {
  "stays below zero"
} else {
  "crosses zero"
}

sig_line <- if (p_val < 0.05) {
  paste0(
    "Because the 95% confidence interval (", tr_ci, " °C/decade) ", ci_note,
    " and p is ", tr_p, " (< 0.05), the trend is statistically significant—pointing to a real average trend rather than random noise alone."
  )
} else {
  paste0(
    "Because the 95% confidence interval (", tr_ci, " °C/decade) ", ci_note,
    " and p is ", tr_p, " (>= 0.05), the trend is not statistically significant—year-to-year swings could easily produce a slope like this with no true trend."
  )
}

r2_pct_txt  <- sprintf("%.1f%%", r2_val * 100)
rem_pct_txt <- sprintf("%.1f%%", (1 - r2_val) * 100)

cutoff_note <- if (yr_to == 2026L && max(model_df$year) < 2026L) {
  " (Data runs through Oct 31, 2026; incomplete 2026 is excluded.)"
} else if (max(model_df$year) == 2026L) {
  " (Data runs through Oct 31, 2026; complete years only.)"
} else {
  ""
}

main_summary <- paste0(
  city_label_use, " (", selected_variable, ", ", selected_source, ", ",
  min(model_df$year), "–", max(model_df$year), ") shows an average linear trend of ",
  tr_slope, " °C per decade. ", sig_line,
  " The straight line accounts for ", r2_pct_txt,
  " of the year-to-year temperature variation (R² = ", tr_r2,
  "), leaving ", rem_pct_txt, " to other weather and climate fluctuations.",
  cutoff_note
)

dw_note_txt <- if (dw_stat >= 1.5 && dw_stat <= 2.5) {
  "little lag-1 carry-over between adjacent years"
} else {
  "possible lag-1 carry-over between adjacent years"
}

spread_note_txt <- if (abs(abs_resid_cor) < 0.3) {
  "even spread across fitted temperatures"
} else {
  "shifting spread across fitted temperatures"
}

norm_note_txt <- if (!is.null(shapiro_out) && shapiro_out$p.value >= 0.05) {
  "no strong evidence against normality"
} else if (!is.null(shapiro_out)) {
  "some evidence against normality"
} else {
  "normality test skipped"
}

assumptions_summary <- paste0(
  "Assumption checks (read with the diagnostic plots): independence (", dw_metric,
  " -> ", dw_note_txt, "; near 2.0 is ideal), spread vs. fitted (|r| = ", abs_cor_txt,
  " -> ", spread_note_txt, "), and normality (Shapiro-Wilk p = ", shapiro_p_txt,
  " -> ", norm_note_txt, ")."
)

tr_text <- paste(main_summary, assumptions_summary, sep = "\n\n")

cat("--- What this means (tr_text) ---\n")
cat(tr_text, "\n\n")

