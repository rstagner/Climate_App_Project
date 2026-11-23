# data_raw/pull_nasa_power.R
# Pull daily NASA POWER data for cities in data/city_stations.rds with preliminary table shaping.
# Run from the project root directory (Climate_App_Project).
# Writes a single dated .rds file in data_raw/ with the pull date saved inside.

library(dplyr)
library(purrr)
library(httr2)
library(jsonlite)

# Pull settings - adjust before running
start_date <- "19900101"
end_date   <- format(Sys.Date(), "%Y%m%d")
community  <- "AG"
parameters <- c("T2M", "T2M_MAX", "T2M_MIN")
pause_sec  <- 0.25
max_cities <- NULL # Set to e.g. 3 for a quick preliminary test; NULL pulls all 48

# Match whatever class GEOID has in city_stations.rds
cast_to_class <- function(x, target_class) {
  if (target_class == "factor") {
    factor(as.character(x))
  } else if (target_class == "integer") {
    as.integer(x)
  } else if (target_class == "numeric" || target_class == "double") {
    as.numeric(x)
  } else if (target_class == "character") {
    as.character(x)
  } else {
    methods::as(x, target_class)
  }
}

# Load city_stations and validate the 48-city source table
stations_path <- "data/city_stations.rds"
stopifnot(file.exists(stations_path))

city_stations <- readRDS(stations_path)
geoid_class   <- class(city_stations$GEOID)[1]

stopifnot(
  nrow(city_stations) == 48,
  !anyNA(city_stations$GEOID),
  !anyNA(city_stations$LAT),
  !anyNA(city_stations$LON)
)

city_stations <- city_stations |>
  mutate(
    GEOID    = cast_to_class(GEOID, geoid_class),
    city_key = paste(as.character(GEOID), CITY, STATE, sep = "_")
  )

pull_stations <- if (!is.null(max_cities)) {
  head(city_stations, max_cities)
} else {
  city_stations
}

# Pull daily point data for one city and shape into a preliminary tibble
fetch_nasa_power_prelim <- function(lat, lon, geoid, geoid_cls, city, state, region,
                                    station_info_id, start, end, comm, pars,
                                    pull_date, pause = 0.25) {
  url <- "https://power.larc.nasa.gov/api/temporal/daily/point"
  
  req <- request(url) |>
    req_url_query(
      parameters = paste(pars, collapse = ","),
      community  = comm,
      latitude   = round(lat, 4),
      longitude  = round(lon, 4),
      start      = start,
      end        = end,
      format     = "JSON"
    ) |>
    req_timeout(30) |>
    req_retry(max_tries = 3, backoff = ~ 2)
  
  resp <- req_perform(req)
  body <- resp_body_json(resp, simplifyVector = TRUE)
  
  param_list <- body$properties$parameter
  if (is.null(param_list) || length(param_list) == 0) {
    stop("API response missing properties$parameter")
  }
  
  dates <- names(param_list[[1]])
  
  df_out <- tibble(
    GEOID           = cast_to_class(geoid, geoid_cls),
    CITY            = as.character(city),
    STATE           = as.character(state),
    REGION          = as.character(region),
    station_info_id = station_info_id,
    query_lat       = as.numeric(lat),
    query_lon       = as.numeric(lon),
    pulled_date     = pull_date,
    DATE_YYYYMMDD   = dates
  )
  
  for (p in names(param_list)) {
    df_out[[p]] <- as.numeric(param_list[[p]][dates])
  }
  
  Sys.sleep(pause)
  df_out
}

safe_fetch_prelim <- safely(fetch_nasa_power_prelim, otherwise = NULL)

# Run across selected cities using a single pull_date
pull_date  <- Sys.Date()
city_rows  <- split(pull_stations, seq_len(nrow(pull_stations)))
names(city_rows) <- pull_stations$city_key

results <- map(city_rows, function(row) {
  safe_fetch_prelim(
    lat             = row$LAT[[1]],
    lon             = row$LON[[1]],
    geoid           = row$GEOID[[1]],
    geoid_cls       = geoid_class,
    city            = row$CITY[[1]],
    state           = row$STATE[[1]],
    region          = row$REGION[[1]],
    station_info_id = row$station_info_id[[1]],
    start           = start_date,
    end             = end_date,
    comm            = community,
    pars            = parameters,
    pull_date       = pull_date,
    pause           = pause_sec
  )
})

raw_list  <- map(results, "result")
err_list  <- map(results, "error")
succeeded <- names(raw_list)[!map_lgl(raw_list, is.null)]
failed    <- names(raw_list)[map_lgl(raw_list, is.null)]

message(sprintf(
  "[NASA POWER Pull] Date: %s | Succeeded: %d / %d cities | Failed: %d",
  as.character(pull_date), length(succeeded), nrow(pull_stations), length(failed)
))

if (length(failed) > 0) {
  message("[Warning] Failed cities:")
  for (nm in failed) {
    err_msg <- if (!is.null(err_list[[nm]])) conditionMessage(err_list[[nm]]) else "Unknown error"
    message("  - ", nm, ": ", err_msg)
  }
}

stopifnot(length(succeeded) > 0)

# Save single dated .rds file in data_raw/
dir.create("data_raw", showWarnings = FALSE, recursive = TRUE)
out_path <- sprintf("data_raw/nasa_power_raw_%s.rds", pull_date)

combined_tbl <- bind_rows(compact(raw_list)) |>
  mutate(GEOID = cast_to_class(GEOID, geoid_class))

nasa_power_raw <- list(
  pulled_date = pull_date,
  pulled_at   = Sys.time(),
  start_date  = start_date,
  end_date    = end_date,
  community   = community,
  parameters  = parameters,
  geoid_class = geoid_class,
  by_city     = compact(raw_list),
  combined    = combined_tbl
)

saveRDS(nasa_power_raw, out_path)

size_mb    <- file.info(out_path)$size / (1024 * 1024)
total_rows <- nrow(nasa_power_raw$combined)

message(sprintf("[Success] Saved RDS output to '%s'", out_path))
message(sprintf("[File Info] Size: %.2f MB | Total Rows: %s",
                size_mb, format(total_rows, big.mark = ",")))

# compare old vs new raw nasa rds to check changes

nasa_old_raw <- readRDS("data_raw/nasa_power_raw_2026-11-19.rds")
nasa_new_raw <- readRDS("data_raw/nasa_power_raw_2026-11-22.rds")

as_tidy_df <- function(x) {
  if (is.data.frame(x)) {
    return(x)
  }
  if (is.list(x)) {
    # Keep data.frame elements; if a city name is stored in names(x), attach it
    dfs <- lapply(seq_along(x), function(i) {
      el <- x[[i]]
      if (is.data.frame(el)) {
        if (!is.null(names(x)) && nzchar(names(x)[i]) && !("city" %in% names(el)) && !("CITY" %in% names(el))) {
          el$city <- names(x)[i]
        }
        return(el)
      }
      NULL
    })
    dfs <- Filter(Negate(is.null), dfs)
    if (length(dfs) > 0) {
      return(bind_rows(dfs))
    }
  }
  stop("Could not coerce object to a data.frame — inspect str(x) to see its structure.")
}

nasa_old <- as_tidy_df(nasa_old_raw)
nasa_new <- as_tidy_df(nasa_new_raw)

city_col_old <- intersect(c("CITY", "city", "urban_area", "name"), names(nasa_old))[1]
city_col_new <- intersect(c("CITY", "city", "urban_area", "name"), names(nasa_new))[1]

if (is.na(city_col_old) || is.na(city_col_new)) {
  cat("Available columns in nasa_old:", paste(names(nasa_old), collapse = ", "), "\n")
  cat("Available columns in nasa_new:", paste(names(nasa_new), collapse = ", "), "\n")
  stop("City column not found — update the candidate column names above.")
}

meta_cols_old <- intersect(c("GEOID", "geoid", city_col_old, "STATE", "state", "REGION", "region"), names(nasa_old))
meta_cols_new <- intersect(c("GEOID", "geoid", city_col_new, "STATE", "state", "REGION", "region"), names(nasa_new))

cities_old <- nasa_old |>
  distinct(across(all_of(meta_cols_old))) |>
  mutate(.city_key = .data[[city_col_old]])

cities_new <- nasa_new |>
  distinct(across(all_of(meta_cols_new))) |>
  mutate(.city_key = .data[[city_col_new]])

old_names <- unique(na.omit(cities_old$.city_key))
new_names <- unique(na.omit(cities_new$.city_key))

removed_cities <- setdiff(old_names, new_names)
added_cities   <- setdiff(new_names, old_names)
kept_cities    <- intersect(old_names, new_names)

cat("\n================================================================\n")
cat("NASA RAW PULL CITY COMPARISON (2026-11-19 vs. 2026-11-22)\n")
cat("================================================================\n")
cat("Distinct cities in 2026-11-19 pull:", length(old_names), "\n")
cat("Distinct cities in 2026-11-22 pull:", length(new_names), "\n")
cat("Cities kept across both pulls:      ", length(kept_cities), "\n")
cat("Removed cities (in old, not new):   ", length(removed_cities), "\n")
cat("Added cities (in new, not old):     ", length(added_cities), "\n")

cat("\n--- Removed Cities (Present in 2026-11-19, Missing in 2026-11-22) ---\n")
if (length(removed_cities) > 0) {
  print(
    cities_old |>
      filter(.city_key %in% removed_cities) |>
      select(-.city_key),
    n = Inf
  )
} else {
  cat("None\n")
}

cat("\n--- Added / Replacement Cities (Present in 2026-11-22, Not in 2026-11-19) ---\n")
if (length(added_cities) > 0) {
  print(
    cities_new |>
      filter(.city_key %in% added_cities) |>
      select(-.city_key),
    n = Inf
  )
} else {
  cat("None\n")
}

if (length(added_cities) > 0 && "date" %in% names(nasa_new)) {
  cat("\n--- Row Counts & Date Coverage for Added Cities (2026-11-22 Pull) ---\n")
  print(
    nasa_new |>
      filter(.data[[city_col_new]] %in% added_cities) |>
      summarise(
        n_rows   = n(),
        min_date = min(as.Date(date), na.rm = TRUE),
        max_date = max(as.Date(date), na.rm = TRUE),
        .by = all_of(intersect(c(city_col_new, "GEOID", "geoid", "REGION", "region"), names(nasa_new)))
      ),
    n = Inf
  )
}


