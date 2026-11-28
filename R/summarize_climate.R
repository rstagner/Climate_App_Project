library(tidyverse)

climate <- readRDS("data/climate_daily.rds")

#----------------------
#creating the annual data for both noaa and nasa data
climate_annual <- climate |> 
  #count days first, before mean is replaced by yearly average
  summarise(noaa_days = sum(!is.na(noaa_mean)),
            #yearly averages of daily temperatures
            #--- NOAA ----
            noaa_mean = mean(noaa_mean, na.rm = TRUE),
            noaa_max = mean(noaa_max, na.rm = TRUE),
            noaa_min = mean(noaa_min, na.rm = TRUE),
            noaa_precip_days = sum(!is.na(noaa_precip)), #count precip days before summing
            noaa_precip = if (noaa_precip_days >= 330) sum(noaa_precip, na.rm = TRUE) else NA_real_,
            noaa_hdd = sum(noaa_hdd, na.rm = TRUE),
            noaa_cdd = sum(noaa_cdd, na.rm = TRUE),
            noaa_heatwave_days = sum(noaa_heatwave),
            #-----NASA --- 
            nasa_days = sum(!is.na(nasa_mean)),
            nasa_mean = mean(nasa_mean, na.rm = TRUE),
            nasa_max = mean(nasa_max, na.rm = TRUE),
            nasa_min = mean(nasa_min, na.rm = TRUE),
            nasa_hdd = sum(nasa_hdd, na.rm = TRUE),
            nasa_cdd = sum(nasa_cdd, na.rm = TRUE),
            nasa_heatwave_days = sum(nasa_heatwave),
            .by = c(GEOID, city, state, region, decade, year))

#check 
nrow(climate_annual) #should be 1776
#check for incomplete years
climate_annual |> filter(noaa_days < 330) |> count(year)

#------------------
# creating monthly data 
climate_monthly <- climate |> 
  #count days first, before mean is replaced by yearly average
  summarise(noaa_days = sum(!is.na(noaa_mean)),
            #yearly averages of daily temperatures
            #--- NOAA ----
            noaa_mean = mean(noaa_mean, na.rm = TRUE),
            noaa_max = mean(noaa_max, na.rm = TRUE),
            noaa_min = mean(noaa_min, na.rm = TRUE),
            noaa_precip_days = sum(!is.na(noaa_precip)), #count precip days before summing
            noaa_precip = if (noaa_precip_days >= 27) sum(noaa_precip, na.rm = TRUE) else NA_real_,
            noaa_hdd = sum(noaa_hdd, na.rm = TRUE),
            noaa_cdd = sum(noaa_cdd, na.rm = TRUE),
            noaa_heatwave_days = sum(noaa_heatwave),
            #-----NASA --- 
            nasa_days = sum(!is.na(nasa_mean)),
            nasa_mean = mean(nasa_mean, na.rm = TRUE),
            nasa_max = mean(nasa_max, na.rm = TRUE),
            nasa_min = mean(nasa_min, na.rm = TRUE),
            nasa_hdd = sum(nasa_hdd, na.rm = TRUE),
            nasa_cdd = sum(nasa_cdd, na.rm = TRUE),
            nasa_heatwave_days = sum(nasa_heatwave),
            .by = c(GEOID, city, state, region, decade, year, month))
#monthly checks
nrow(climate_monthly)  #about 21216
climate_monthly |> filter(noaa_days < 27) |> count(month)    #incomplete months


#save the files
saveRDS(climate_annual, "data/climate_annual.rds")
saveRDS(climate_monthly, "data/climate_monthly.rds")

#check file sizes
file.size("data/climate_annual.rds") / 1e6
file.size("data/climate_monthly.rds") / 1e6
