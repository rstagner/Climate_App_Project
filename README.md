# U.S. Cities Climate Explorer

DATA-413/613 Group Project: Local Climate Trends Shiny App

Group members: Prashant Choudhary, Brook Estifanos, Rachel Stagner

Use case: Help high school science teachers and students explore how local climate has changed in U.S. cities since 1990, and how well satellite-modeled climate data matches what ground stations actually recorded.

Live app: https://01a21dcf-8af9-0d09-afc3-f36f909e93a7.share.connect.posit.cloud/

## Data sources

| Source | What we use | How we get it |
|---|---|---|
| NASA POWER | Daily mean, max, and min air temperature at 2 m (T2M, T2M_MAX, T2M_MIN), modeled from satellite and reanalysis data | REST API (httr2 + jsonlite), `data_raw/pull_nasa_power.R` |
| NOAA GHCN-Daily | Daily max and min temperature and precipitation from weather stations | Raw station files from NCEI, `data_raw/pull_noaa_daily.R` |
| U.S. Census Bureau | 2026 urban area Gazetteer (names, coordinates) and urban area population (API) | Gazetteer file in `data_raw/`, population from `data_raw/pull_census_ua_pop.R` |

Coverage: 48 cities (12 per Census region), January 1, 1990 to October 31, 2026. Data pulled November 2026 (see dates in `data_raw/` file names).

## Folder layout

- `data_raw/` raw downloads and the scripts that pull them. Raw files are never edited.
- `R/` scripts that clean, match, join, and summarize the data. Also early prototype scripts from building the app panels.
- `data/` cleaned and derived data (`.rds`, plus `.csv` copies of the city lists)
- `app/` the Shiny app. Only this folder is deployed.
  - `app/app.R` UI and server
  - `app/R/` helper functions for each Statistics panel (Shiny loads these automatically)
  - `app/data/` copies of the data the app reads (made by `R/copy_app_data.R`)

## How to rebuild the data

Run from the project root (open `Climate_App_Project.Rproj` first so paths work). The Census API pull needs a key saved in `.Renviron` as `CENSUS_API_KEY`. Never put the key in a script.

**1. Cities**

1. `R/urban_areas_clean.R` reads the Gazetteer file, saves `data/urban_areas_clean.rds`
2. `data_raw/pull_census_ua_pop.R` pulls urban area population from the Census API
3. `R/census_data_cleaning.R` cleans the population data, saves `ua_pop_clean.rds`
4. `R/select_cities.R` joins the two, adds Census regions, keeps the 15 largest urban areas per region, saves `data/city_pool.rds` (60 candidates)

**2. Weather stations**

5. `data_raw/pull_noaa_stations.R` downloads the GHCN-Daily station list and inventory
6. `R/match_stations.R` finds stations within 25 km of each candidate city (haversine distance), saves `data/station_candidates.rds`
7. `data_raw/pull_noaa_daily.R` downloads daily records for every candidate station, saves `data_raw/noaa_raw_candidates_<date>.rds`
8. `R/station_completeness.R` checks how complete each station's temperature and precipitation records are, saves `data/candidate_completeness.rds`
9. `R/select_final_stations.R` keeps stations with at least 90% of temperature days, prefers Weather Service (USW) stations, then the nearest, and keeps the 12 largest cities per region. Saves `data/city_stations.rds` and `.csv` (48 cities)

**3. Climate data**

10. `R/noaa_tidy.R` removes quality-flagged values, converts units, computes the daily mean, saves `data/noaa_tidy.rds`
11. `data_raw/pull_nasa_power.R` pulls NASA POWER for the 48 city points
12. `R/nasa_power_clean.R` tidies the NASA data, saves `data/nasa_power_tidy.rds`
13. `R/join_data.R` joins NASA and NOAA by city and date, ends at 2026-10-31, saves `data/climate_joined.rds`
14. `R/derive-variables.R` adds decade, heating and cooling degree days, and heat-wave days, saves `data/climate_daily.rds`
15. `R/summarize_climate.R` makes annual and monthly summaries, saves `data/climate_annual.rds` and `data/climate_monthly.rds`
16. `R/copy_app_data.R` copies the files the app needs into `app/data/`. Rerun this any time upstream data changes.

See `data_dictionary.md` for every column.

## Running and deploying the app

Run locally: open `app/app.R` and click Run App.

Deploy (from the project root):

```r
rsconnect::deployApp("app", appName = "us-cities-climate-explorer")
```

Set up the shinyapps.io account with `rsconnect::setAccountInfo()` in the console only. Do not save the token in a script or commit it.

## Methods notes

- Cities are Census urban areas. The city point is the urban area's internal point from the Gazetteer, so it can fall outside the city limits (for example, Washington's is in northern Virginia).
- Years and months with too few days are left out of summaries and tests: 330 days per year, 27 per month. Precipitation has its own day count because some stations are missing precipitation even when temperature is complete (Detroit has no precipitation before 2001).
- Station selection required 90% complete temperature, not precipitation.
- NOAA daily mean is (TMAX + TMIN) / 2. NASA POWER daily mean is its own 24-hour average (T2M). This difference is part of the satellite vs. station gap.
- Precipitation is NOAA only.
- Anomalies, heat-wave thresholds, and normals use 1991 to 2020, the current 30-year climate normal period (WMO, NOAA).
- Heat-wave day: part of a run of 2 or more days with daily max above that city's 1991 to 2020 90th percentile.
- Degree days use a base of 18.3 °C (65 °F).