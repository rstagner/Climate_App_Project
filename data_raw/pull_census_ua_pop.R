library(httr2)
library(tidyverse)

#load aptidyverse#load api key
census_key <- Sys.getenv("CENSUS_API_KEY")

#load url for API pull
url <- "https://api.census.gov/data/2020/dec/dhc"

#setup pull
resp <- request(url) |> 
  req_url_query(
    get = "NAME,P1_001N", #name and population
    `for` = "urban area:*", #all urban areas
    key = census_key
  ) |> 
  req_perform()

#save the response with the date
out_file <- paste0("../data_raw/census_ua_pop_", Sys.Date(), ".json")

#save the response in the file
writeLines(resp_body_string(resp), out_file)

#check and store data
raw <- jsonlite::fromJSON(out_file)
dim(raw)
head(raw)


