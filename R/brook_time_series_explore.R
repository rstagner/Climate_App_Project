climate_joined <- readRDS("data/climate_joined.rds")

# Print exact column names
names(climate_joined)

# inspect column types and first few rows
dplyr::glimpse(climate_joined)
