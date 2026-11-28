# Data Dictionary

Units: temperatures in °C, precipitation in mm, distances in km. `noaa_` columns come from the matched NOAA weather station, `nasa_` columns come from NASA POWER at the city point.

## city_stations (`data/city_stations.rds`, `.csv`)

One row per city (48 rows). Made by `R/select_final_stations.R`.

| Column | Description |
|----|----|
| REGION | Census region (Northeast, Midwest, South, West) |
| CITY | City name from the urban area name |
| STATE | State abbreviation |
| LAT, LON | Urban area internal point from the 2026 Census Gazetteer |
| GEOID | Census urban area code. Used to join every file. |
| POP | Urban area population (Census API) |
| station_info_id | GHCN-Daily station ID |
| station_name | Station name |
| station_state | Station state |
| station_lat, station_long | Station coordinates |
| dist_km | Distance from the urban area point to the station (haversine) |
| is_wban | TRUE if a Weather Service station (ID starts with USW) |
| temp_pct | Share of days 1990 to 2026 with temperature data |
| precip_pct | Share of days 1990 to 2026 with precipitation data |

## climate_joined (`data/climate_joined.rds`)

One row per city per day, January 1, 1990 to October 31, 2026. Made by `R/join_data.R`.

| Column | Description |
|----|----|
| GEOID | Urban area code |
| date | Date |
| nasa_mean, nasa_max, nasa_min | NASA POWER daily mean, max, min temperature at 2 m (T2M, T2M_MAX, T2M_MIN) |
| station_id | GHCN-Daily station ID |
| noaa_mean | (noaa_max + noaa_min) / 2 |
| noaa_max, noaa_min | Station daily max and min temperature (TMAX, TMIN) |
| noaa_precip | Station daily precipitation (PRCP) |
| city, state, region, lat, lon | From city_stations |
| year, month | From date |

NOAA values with a quality flag were removed, so missing days are NA.

## climate_daily (`data/climate_daily.rds`)

climate_joined plus derived variables. Made by `R/derive-variables.R`. `noaa_` and `nasa_` versions of each:

| Column | Description |
|----|----|
| decade | "1990s", "2000s", "2010s", "2020s" |
| noaa_hdd, nasa_hdd | Heating degree days: max(18.3 - daily mean, 0) |
| noaa_cdd, nasa_cdd | Cooling degree days: max(daily mean - 18.3, 0) |
| noaa_hw_threshold, nasa_hw_threshold | City's 90th percentile of daily max, 1991 to 2020 |
| noaa_hot, nasa_hot | TRUE if daily max is above the threshold (missing days are FALSE) |
| noaa_heatwave, nasa_heatwave | TRUE if a hot day is part of a run of 2 or more hot days |

## climate_annual (`data/climate_annual.rds`)

One row per city per year (1,776 rows). Made by `R/summarize_climate.R`. 2026 is a partial year.

| Column | Description |
|----|----|
| GEOID, city, state, region, decade, year | Identifiers |
| noaa_days, nasa_days | Days with a daily mean temperature. Use years with 330 or more. |
| noaa_mean, noaa_max, noaa_min | Average of the daily values for the year |
| noaa_precip_days | Days with precipitation data |
| noaa_precip | Total precipitation for the year. NA if fewer than 330 precipitation days. |
| noaa_hdd, noaa_cdd | Total heating and cooling degree days for the year |
| noaa_heatwave_days | Number of heat-wave days in the year |
| nasa_mean, nasa_max, nasa_min, nasa_hdd, nasa_cdd, nasa_heatwave_days | Same as the NOAA columns, from NASA POWER |

There is no NASA precipitation.

## climate_monthly (`data/climate_monthly.rds`)

One row per city per month (about 21,216 rows). Same columns as climate_annual, plus `month`, summarized by month instead of year. Use months with 27 or more days. `noaa_precip` is NA if fewer than 27 precipitation days.

## Other files in `data/`

| File | Description |
|----|----|
| urban_areas_clean.rds | Cleaned Census urban area Gazetteer |
| ua_pop_clean.rds | Urban area population from the Census API |
| city_pool.rds, .csv | 60 candidate cities (15 largest per region) |
| station_candidates.rds | Stations within 25 km of each candidate city |
| candidate_completeness.rds | Temperature and precipitation completeness for each candidate station |
| noaa_completeness.rds, .csv | Completeness check from the first station list (replaced by candidate_completeness) |
| noaa_tidy.rds | Cleaned NOAA daily data for the final 48 stations |
| nasa_power_tidy.rds | Cleaned NASA POWER daily data for the 48 cities |
