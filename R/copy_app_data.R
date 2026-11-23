#copy the files the Shiny app needs into app/data 
#this step is needed since when the Shiny app is uploaded, it will only
#uploade what's in the app folder, so there needs to be a /data folder to ensure
#the data the app needs is there and the relative paths will work

#IMPORTANT: This file must be rerun any time there are upstream data changes

dir.create("app/data", showWarnings = FALSE) #creates the folder

app_files <- c(
  "data/city_stations.rds",
  "data/climate_joined.rds"
  #add the joined data and summary tables here once they are built
)

file.copy(app_files, "app/data/", overwrite = TRUE)

#check work
list.files("app/data")
