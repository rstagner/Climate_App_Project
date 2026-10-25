# Climate_App_Project
DATA-413/613 Group Project Plan: Local Climate Trends Shiny App	
	
Use case: Help high school science teachers and students explore how local climate has changed in U.S. cities since 1990, and how well satellite-modeled climate data matches what ground stations actually recorded.

Data source 1: NASA POWER daily point data, pulled via the raw REST API (httr2 + jsonlite)

Data source 2: NOAA GHCN-Daily weather station records (raw fixed-width files)

Data source 3: U.S. Census Bureau Gazetteer files (city names and coordinates)

Scope: About 30 to 50 cities, 1990 to present, 4 to 5 variables (daily mean, max, min temperature; precipitation; maybe humidity)

Group members: Prashant Choudhary, Brook Estifanos, Rachel Stagner
