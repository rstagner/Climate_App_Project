library(dplyr)
library(tidyr)
library(ggplot2)

# =========================================================================
# 1. PARAMETERS
# =========================================================================
data_path <- "data/nasa_power_tidy.rds"

# Choose which temperature variable to highlight in the boxplot & summary:
# "nasa_mean", "nasa_max", or "nasa_min"
target_col <- "nasa_mean"

# Optional date filter (leave end_date NULL to stop at the last date with
# non-missing values and avoid the recent tail NAs from 2026-11-17 onward)
start_date <- as.Date("1990-01-01")
end_date   <- NULL

# =========================================================================
# 2. LOAD & PREPARE DATA (ALL CITIES)
# =========================================================================
if (!file.exists(data_path)) {
  stop(sprintf("File not found: '%s'. Run your NASA tidy script first.", data_path))
}

nasa_tidy <- readRDS(data_path) |>
  mutate(
    date       = as.Date(date),
    city_label = paste0(city, ", ", state)
  )

if (!target_col %in% names(nasa_tidy)) {
  stop(sprintf("Column '%s' not found in %s.", target_col, data_path))
}

if (is.null(end_date)) {
  end_date <- nasa_tidy |>
    filter(!is.na(.data[[target_col]])) |>
    summarise(max_dt = max(date, na.rm = TRUE)) |>
    pull(max_dt)
}

nasa_filtered <- nasa_tidy |>
  filter(date >= start_date, date <= end_date) |>
  mutate(selected_temp = .data[[target_col]])

# =========================================================================
# 3. SUMMARY TABLE (BY CITY)
# =========================================================================
nasa_summary <- nasa_filtered |>
  group_by(GEOID, city_label) |>
  summarise(
    valid_days  = sum(!is.na(selected_temp)),
    min_temp    = suppressWarnings(min(selected_temp, na.rm = TRUE)),
    median_temp = median(selected_temp, na.rm = TRUE),
    mean_temp   = mean(selected_temp, na.rm = TRUE),
    max_temp    = suppressWarnings(max(selected_temp, na.rm = TRUE)),
    sd_temp     = sd(selected_temp, na.rm = TRUE),
    pct_na      = round(mean(is.na(selected_temp)) * 100, 2),
    .groups     = "drop"
  ) |>
  mutate(
    min_temp    = ifelse(is.infinite(min_temp), NA_real_, round(min_temp, 2)),
    median_temp = round(median_temp, 2),
    mean_temp   = round(mean_temp, 2),
    max_temp    = ifelse(is.infinite(max_temp), NA_real_, round(max_temp, 2)),
    sd_temp     = round(sd_temp, 2)
  )

cat(sprintf(
  "\n--- Summary for '%s' (%s to %s) ---\n",
  target_col, start_date, end_date
))
print(nasa_summary, n = Inf)

# =========================================================================
# 4. BOXPLOT BY CITY
# =========================================================================
plot_boxplot <- ggplot(
  nasa_filtered,
  aes(
    x    = reorder(city_label, selected_temp, FUN = median, na.rm = TRUE),
    y    = selected_temp,
    fill = city_label
  )
) +
  geom_boxplot(alpha = 0.85, outlier.alpha = 0.2, show.legend = FALSE, na.rm = TRUE) +
  labs(
    title    = sprintf("Daily %s Distribution by City", target_col),
    subtitle = sprintf("NASA POWER (%s to %s), ordered by median", start_date, end_date),
    x        = "City (ordered by median)",
    y        = sprintf("%s (°C)", target_col)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.text.x = element_text(angle = 65, hjust = 1, size = 8)
  )

print(plot_boxplot)

