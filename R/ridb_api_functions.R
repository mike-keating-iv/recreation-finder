# Author: Mike Keating
# Suite of functions for querying the RIDB federal recreation API
# Dependencies

library(httr)
library(jsonlite)
suppressWarnings(library(tidyverse))
library(zipcodeR)

## API Functions
get_ridb <- function(endpoint, params=list()){
  # Base query which we will modify/call for each specific request
  # Each function will pass it a specific endpoint
  base <- "https://ridb.recreation.gov/api/v1"
  url <- paste0(base, endpoint)
  
  # Get local key
  key <- Sys.getenv("RIDB_API_KEY")
  
  res <- GET(url, query= c(apikey=key, params), accept("application/json"))
  all_data <- content(res, as ="parsed", simplifyVector = TRUE)
  #metadata_tibble <- as_tibble(all_data$METADATA$RESULTS)
  recdata_tibble <- as_tibble(all_data$RECDATA)
  return(recdata_tibble)
  
}



get_facilities <- function(state = NULL, activity = NULL, limit = 500, zip_code = NULL, radius_miles = NULL){
  # Create a named list of parameters which we will pass to the httr package
  # This is more elegant than the approach used in hw4
  
  params <- list()
  
  # These parameters are optional for the call
  if (!is.null(state)) params$state <- state
  if (!is.null(activity)) params$activity <- activity
  if (!is.null(zip_code)) {
    zip_lookup <- geocode_zip(zip_code)
    params$longitude <- zip_lookup$lng
    params$latitude <- zip_lookup$lat
  }
  if (!is.null(radius_miles)) params$radius <- radius_miles
  
  # Call our get_ridb helper function
  print(params)
  facs <- get_ridb("/facilities", params)
  
  # Filter
  
  facs <- facs |> select(FacilityName,FacilityDescription, 
                         FacilityID,FacilityLatitude, 
                         FacilityLongitude,FacilityTypeDescription, 
                         FacilityUseFeeDescription,OrgFacilityID,
                         ParentOrgID, ParentRecAreaID, Reservable)
  return(facs)
}

get_campsites_for_facility <- function(facility_id){
  
  endpoint <- paste0("/facilities/", facility_id, "/campsites")
  print(endpoint)
  get_ridb(endpoint)
}

