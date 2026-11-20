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


