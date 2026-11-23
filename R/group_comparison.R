library(tidyverse)
library(car)

climate_annual <- readRDS("data/climate_annual.rds")

#-----DECADES ANALYSIS: are decades different from each other? -------#

#keep complete years only
annual_complete <- climate_annual |> 
  filter(noaa_days >= 330)

#each city's 1991-2020 average (the "normal")
city_normals <- annual_complete |> 
  filter(year >= 1991, year <= 2020) |> 
  summarize(normal = mean(noaa_mean), .by = GEOID)

#anomaly = each year's value minus the city's normal
#averaged per city per decade
decade_data <- annual_complete |> 
  left_join(city_normals, by = "GEOID") |> 
  mutate(anomaly = noaa_mean - normal) |> 
  summarize(anomaly = mean(anomaly), .by = c(GEOID, city, region, decade))

#check work
nrow(decade_data) #should be 192 (48 * 4 decades)
decade_data |>  summarize(mean_anomaly = mean(anomaly), .by = decade)

#ANOVA Tests
decade_model <- aov(anomaly ~ decade, data = decade_data)
summary(decade_model)

#Tukey's test
TukeyHSD(decade_model)

#test assumptions - normality
shapiro.test(residuals(decade_model))

#Q-Q plot of residuals
qqnorm(residuals(decade_model))
qqline(residuals(decade_model))

#test assumptions - spread of data
car::leveneTest(anomaly ~ factor(decade), data = decade_data)

#how spread out is each decade?
decade_data |>  summarize(sd = sd(anomaly), .by = decade)

#Welch's ANOVA - no assumption of equal variance
oneway.test(anomaly ~ decade, data = decade_data, var.equal = FALSE)

#Kruskal-Wallis: rank-based; no assumption of normality
kruskal.test(anomaly ~ decade, data = decade_data)

#examine outliers of Q-Q plot
decade_data |> slice_min(anomaly, n = 4)

#find outliers
decade_data |> 
  mutate(resid = residuals(decade_model)) |> 
  slice_min(resid, n = 4)

#check to see if NASA and NOAA data agree on the outliers (note, they don't
#which is a good example for the satellite & station panel)
annual_complete |> 
  filter(city %in% c("Los Angeles", "San Francisco", "San Diego")) |> 
  group_by(city) |> 
  mutate(noaa_anom = noaa_mean - mean(noaa_mean[year >= 1991 & year <= 2020]),
         nasa_anom = nasa_mean - mean(nasa_mean[year >= 1991 & year <= 2020], na.rm = TRUE)) |> 
  summarise(noaa_2020s = mean(noaa_anom[decade == "2020s"]),
            nasa_2020s = mean(nasa_anom[decade == "2020s"], na.rm = TRUE))

#pairwise comparisons
pairwise.wilcox.test(decade_data$anomaly, decade_data$decade, p.adjust.method = "holm")


#----------------REGIONS analysis: has warming differed by region? --------

#each city's 2020s anomaly: how much warmer than its 1991-2020 normal
region_data <- decade_data |> 
  filter(decade == "2020s")

nrow(region_data) #48, one per city

region_data |> 
  summarise(mean_anomaly = mean(anomaly), 
            sd = sd(anomaly), 
            .by = region)

#ANOVA Tests
region_model <- aov(anomaly ~ region, data = region_data)
summary(region_model)

#Tukey's test
TukeyHSD(region_model)

#test assumptions - normality
shapiro.test(residuals(region_model))

#test assumptions - spread of data
car::leveneTest(anomaly ~ factor(region), data = region_data)

#pairwise comparisons
pairwise.wilcox.test(region_data$anomaly, region_data$region, p.adjust.method = "holm")

#Welch's ANOVA 
oneway.test(anomaly ~ region, data = region_data, var.equal = FALSE)
kruskal.test(anomaly ~ region, data = region_data)

install.packages("rstatix")   #once
rstatix::games_howell_test(region_data, anomaly ~ region)
