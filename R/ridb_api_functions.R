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

  return(all_data$RECDATA)
  
}



get_facilities <- function(state = NULL, activity = NULL, limit = 500, zip_code = NULL, radius_miles = NULL, full = TRUE){
  params <- list(limit = limit) 
  if (full) params$full = "true"  
  if (!is.null(state)) params$state <- state
  if (!is.null(activity)) params$activity <- activity
  if (!is.null(zip_code)) {
    zip_lookup <- geocode_zip(zip_code)
    params$longitude <- zip_lookup$lng
    params$latitude <- zip_lookup$lat
  }
  if (!is.null(radius_miles)) params$radius <- radius_miles
  

  facs <- get_ridb("/facilities", params)
  
  # If no facilities found, return an empty tibble
  if (length(facs) == 0) {
    return(tibble(FacilityID = character(), FacilityName = character(), FacilityDescription = character(),
                  FacilityTypeDescription = character(), ParentOrgID = character(), ParentRecAreaID = character(),
                  FacilityLatitude = numeric(), FacilityLongitude = numeric(), ACTIVITY = list(), ORGANIZATION = list()))
  }
  
  # Return a clean tibble
  facilities_df <- facs |> 
    as_tibble() |>
    select(FacilityID, FacilityName, FacilityDescription, FacilityTypeDescription,
           ParentOrgID, ParentRecAreaID, FacilityLatitude, FacilityLongitude,
           ACTIVITY, ORGANIZATION, CAMPSITE, RECAREA) |>
    mutate(
      # Extract info from the dataframe/list columns
      OrgName = map_chr(ORGANIZATION, function(x) {
        if (is.data.frame(x) && "OrgName" %in% names(x)) {
          paste(unique(x$OrgName), collapse = ", ")
        } else {
          NA_character_
        }
      }),
      OrgID = map_chr(ORGANIZATION, function(x) {
        if (is.data.frame(x) && "OrgID" %in% names(x)) {
          paste(unique(x$OrgID), collapse = ", ")
        } else {
          NA_character_
        }
      }),
      OrgType = map_chr(ORGANIZATION, function(x) {
        if (is.data.frame(x) && "OrgType" %in% names(x)) {
          paste(unique(x$OrgType), collapse = ", ")
        } else {
          NA_character_
        }
      }),
      Activities = map_chr(ACTIVITY, function(x) {
        if (is.data.frame(x) && "ActivityName" %in% names(x)) {
          # Combine all activities into one string
          paste(unique(x$ActivityName), collapse = ", ")
        } else {
          NA_character_
        }
      }),
      RecAreaName = map_chr(RECAREA, function(x) {
        if (is.data.frame(x) && "RecAreaName" %in% names(x)) {
          paste(unique(x$RecAreaName), collapse = ", ")
        } else {
          NA_character_
        }
      }),
    )
  return(facilities_df)
}



## Get all activities.
# We will use this to populate a dropdown list to filter by option and likely be part of loadup
# Counting this as one of the 6 query requirements
get_activities <- function(){
  acts <- get_ridb("/activities")
 
  return(acts)
}



get_campsites_for_facility <- function(facility_id){
  
  endpoint <- paste0("/facilities/", facility_id, "/campsites")
  print(endpoint)
  get_ridb(endpoint)
}

