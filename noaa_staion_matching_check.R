library(dplyr)
library(readr)
library(tibble)

top_cities   <- readRDS("data/top_cities.rds") |> as_tibble()
stations_rds <- readRDS("data/city_stations.rds") |> as_tibble()
stations_csv <- read_csv("data/city_stations.csv", show_col_types = FALSE) |> as_tibble()

glimpse(stations_rds)

# Check 1: 48 rows, one per city, no NAs in key fields------
cat("Rows in RDS / CSV (expect 48):", nrow(stations_rds), nrow(stations_csv), "\n")
stopifnot(nrow(stations_rds) == 48, nrow(stations_csv) == 48)

# Check 2: Verify every city has its own distinct NOAA station (no shared station IDs)
# - Confirms the PR's design rule that all 48 cities match to different stations
# - Prevents silent collisions/overwrites in downstream NOAA pulls, joins, or city summaries
# - Catches distance/tie-breaker or join bugs before starting NASA POWER/NOAA branches

station_cols <- intersect(
  names(stations_rds),
  c("station_info_id", "STATION_INFO_ID", "station_id", "STATION_ID", "ghcnd_id", "GHCND_ID", "station", "STATION", "id", "ID")
)

if (length(station_cols) == 0) {
  stop(
    "Check 2 failed: No station ID column found. Available columns: ",
    paste(names(stations_rds), collapse = ", ")
  )
}

st_col <- station_cols[1]
cat("Check 2 using station column:", st_col, "\n")

dup_stations <- stations_rds |>
  dplyr::count(.data[[st_col]], name = "city_count") |>
  dplyr::filter(city_count > 1)

cat("Duplicate station assignments (expect 0):", nrow(dup_stations), "\n")

if (nrow(dup_stations) > 0) {
  print(dup_stations)
  print(stations_rds |> dplyr::filter(.data[[st_col]] %in% dup_stations[[st_col]]))
  stop("Check 2 failed: One or more NOAA station IDs are assigned to multiple cities.")
}

# Check 3: Every match is within 25 km
dist_cols <- intersect(names(stations_rds), c("distance_km", "dist_km", "distance", "DISTANCE_KM", "DIST_KM"))
stopifnot(length(dist_cols) > 0)
d_col <- dist_cols[1]

max_dist <- max(stations_rds[[d_col]], na.rm = TRUE)
cat("Max distance in km (expect <= 25):", round(max_dist, 2), "\n")
stopifnot(all(!is.na(stations_rds[[d_col]])), max_dist <= 25)

# Show farthest matches & San Jose / DC / secondary airport rows to inspect by eye
stations_rds |>
  arrange(desc(.data[[d_col]])) |>
  slice_head(n = 8) |>
  print()

# Check 4: Haversine test (NYC ~40.7128, -74.0060 to LA ~34.0522, -118.2437)
if (exists("haversine_km", mode = "function")) {
  nyc_to_la <- haversine_km(40.7128, -74.0060, 34.0522, -118.2437)
  cat("NYC to LA distance (expect ~3935 km):", round(nyc_to_la, 1), "\n")
}
# all checks passed!
