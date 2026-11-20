library(dplyr)
library(readr)
library(tibble)

top_cities   <- readRDS("data/top_cities.rds") |> as_tibble()
stations_rds <- readRDS("data/city_stations.rds") |> as_tibble()
stations_csv <- read_csv("data/city_stations.csv", show_col_types = FALSE) |> as_tibble()

glimpse(stations_rds)

# Check 1: 48 rows, one per city, no NAs in key fields
cat("Rows in RDS / CSV (expect 48):", nrow(stations_rds), nrow(stations_csv), "\n")
stopifnot(nrow(stations_rds) == 48, nrow(stations_csv) == 48)

# Check 2: No station assigned to more than one city
# (Adjust column name if the PR uses a slightly different station ID column name)
station_cols <- intersect(names(stations_rds), c("station_id", "STATION_ID", "id", "ID", "station", "STATION"))
stopifnot(length(station_cols) > 0)
st_col <- station_cols[1]

dup_stations <- stations_rds |>
  count(.data[[st_col]]) |>
  filter(n > 1)
cat("Duplicate station assignments (expect 0):", nrow(dup_stations), "\n")
stopifnot(nrow(dup_stations) == 0)

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