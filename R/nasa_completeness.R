library(dplyr)
library(lubridate)

tidy_path <- "data/nasa_power_tidy.rds"

nasa_tidy <- readRDS(tidy_path) |>
  mutate(date = as.Date(date))

expected_removed_cities <- c("New Haven", "Riverside", "San Jose")
expected_added_cities   <- c("Harrisburg", "Tucson", "Honolulu")

present_cities <- unique(na.omit(nasa_tidy$city))

still_present_removed <- intersect(expected_removed_cities, present_cities)
missing_added         <- setdiff(expected_added_cities, present_cities)
found_added           <- intersect(expected_added_cities, present_cities)

start_date <- as.Date("1990-01-01")
end_date   <- as.Date("2026-12-31")
obs_max    <- max(nasa_tidy$date, na.rm = TRUE)

effective_end <- min(end_date, obs_max)
expected_days <- as.integer(effective_end - start_date) + 1L

completeness <- nasa_tidy |>
  filter(date >= start_date, date <= end_date) |>
  summarise(
    n_distinct_days = n_distinct(date),
    min_date        = min(date, na.rm = TRUE),
    max_date        = max(date, na.rm = TRUE),
    pct_days        = round(100 * n_distinct(date) / expected_days, 2),
    pct_nasa_mean   = round(100 * sum(!is.na(nasa_mean)) / expected_days, 2),
    pct_nasa_max    = round(100 * sum(!is.na(nasa_max)) / expected_days, 2),
    pct_nasa_min    = round(100 * sum(!is.na(nasa_min)) / expected_days, 2),
    passes_90_pct   = (100 * n_distinct(date) / expected_days) >= 90,
    .by = c(GEOID, city, state)
  ) |>
  arrange(passes_90_pct, pct_days, city)

cat("\n================================================================\n")
cat("CITY REPLACEMENT & 90% COMPLETENESS CHECK\n")
cat("================================================================\n")
cat("File checked:     ", tidy_path, "\n")
cat("Distinct cities:  ", length(present_cities), "\n")
cat("Date window used: ", as.character(start_date), "to", as.character(effective_end), "(", expected_days, "days )\n")

cat("\n--- Removed Cities Check (Should be NONE in new tidy) ---\n")
if (length(still_present_removed) == 0) {
  cat("PASS: None of New Haven, Riverside, San Jose are present.\n")
} else {
  cat("FAIL: Still present ->", paste(still_present_removed, collapse = ", "), "\n")
}

cat("\n--- Added Cities Check (Should include Harrisburg, Tucson, Honolulu) ---\n")
if (length(missing_added) == 0) {
  cat("PASS: All 3 expected cities present ->", paste(found_added, collapse = ", "), "\n")
} else {
  cat("FAIL: Missing expected cities ->", paste(missing_added, collapse = ", "), "\n")
}
if (length(found_added) > 0) {
  print(
    completeness |>
      filter(city %in% found_added) |>
      select(city, state, pct_days, pct_nasa_mean, passes_90_pct, min_date, max_date),
    n = Inf
  )
}

cat("\n--- 90% Completeness Rule Summary ---\n")
cat("Passing (>= 90% days):", sum(completeness$passes_90_pct), "/", nrow(completeness), "\n")
cat("Failing (< 90% days): ", sum(!completeness$passes_90_pct), "/", nrow(completeness), "\n")

failing <- completeness |>
  filter(!passes_90_pct) |>
  select(city, state, pct_days, pct_nasa_mean, min_date, max_date)

if (nrow(failing) > 0) {
  cat("\nCities below 90%:\n")
  print(failing, n = Inf)
} else {
  cat("\nAll cities meet the 90% threshold.\n")
}
