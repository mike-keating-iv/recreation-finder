# Author: Mike Keating
# Suite of functions for querying the RIDB federal recreation API
# Dependencies

library(httr)
library(jsonlite)
suppressWarnings(library(tidyverse))
library(zipcodeR)

## API Functions
get_ridb <- function(endpoint, params=list(), limit = 500, all_pages = TRUE){
  
  
  # Base query which we will modify/call for each specific request
  # Each function will pass it a specific endpoint
  base <- "https://ridb.recreation.gov/api/v1"
  url <- paste0(base, endpoint)
  
  # Get local key
  key <- Sys.getenv("RIDB_API_KEY")
  

  all_results <- list()
  start <- 0
  repeat{
    page_params <- c(params, list(apikey=key, limit=limit, offset = start))
    
    res <- GET(url, query=page_params, accept("application/json"))
    page_data <- content(res, as ="parsed", simplifyVector = TRUE)
    page_rec_data <-page_data$RECDATA
    
    all_results[[length(all_results) + 1]] <- page_rec_data
    
    total_results <- page_data$METADATA$RESULTS$TOTAL
    
    start <- start + limit
    
    # Stop getting results once we reach no longer add more
    if(!all_pages || start >= total_results) break
    
  }
  

  return(bind_rows(all_results))
  
}


# This should handle 4 out of the 6 query requirements
# Modify state, activity, and zip coordinates (3 modifications, 1 endpoint = 4/6)
get_facilities <- function(state = NULL, activity = NULL, limit = 500, zip_code = NULL, radius_miles = NULL, full = TRUE){
  params <- list(limit = limit) 
  if (full) params$full = "true" # this must be "true" in order for us to return the full dfs for attributes like ORGANIZATION  
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
    select(FacilityID, FacilityName, FacilityDescription, FacilityTypeDescription,Reservable,
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
      CountActivities = map_int(ACTIVITY, function(x) {
        if (is.data.frame(x) && "ActivityName" %in% names(x)) {
          # Combine all activities into one string
          length(unique(x$ActivityName))
        } else {
          # Result must be zero instead of NA in this case
          0
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
# Counting this as one of the 6 query requirements, since it is one endpoint (5/6)
get_activities <- function(){
  acts <- get_ridb("/activities")
  return(acts)
}


# 6/6
# One endpoint
# One end point, one modification
get_campsites_for_facility <- function(facility_id){
  
  endpoint <- paste0("/facilities/", facility_id, "/campsites")
  print(endpoint)
  campsites <- get_ridb(endpoint)
  
  # Handle trying to call this function on a facility that doesn't return campsites
  
  if (nrow(campsites) == 0) {
    campsites <- tibble(Note = "No campsites available for this facility. If 'CAMPING' is listed as an Activity, this may mean open camping is possible instead of offical sites.")
  }
  else{
    # Widen the dataframe by making columns out of each df
    # First use map to make everything into a named list
    campsites <- campsites |> 
      select(CampsiteID, CampsiteName, CampsiteReservable, CampsiteType, TypeOfUse, ATTRIBUTES) |> 
      mutate(ATTRIBUTES_extracted = map(ATTRIBUTES, function(attr_df) {
        
      if (is.data.frame(attr_df) && all(c("AttributeName", "AttributeValue") %in% names(attr_df))) {
        setNames(attr_df$AttributeValue, attr_df$AttributeName)
      } else {
        NULL
      }
    }) # Then expand into columns
    ) |> unnest_wider(ATTRIBUTES_extracted, names_repair = "unique") |> 
      select(!ATTRIBUTES) # drop this column since we don't need it anymore
    
    ## Deprecated
    # campsites <- campsites |>
    #   mutate(
    #     # Flatten attributes into one row per campsite
    #     # It's probably better to widen the tibble and make columns for each
    #   #   AttributeSummary = map_chr(ATTRIBUTES, function(attr_df) {
    #   #     if (is.data.frame(attr_df) && all(c("AttributeName", "AttributeValue") %in% names(attr_df))) {
    #   #       paste0(attr_df$AttributeName, ": ", attr_df$AttributeValue, collapse = "; ")
    #   #     } else {
    #   #       NA_character_
    #   #     }
    #   #   })
    #   # )
    
  }

  return(campsites)
}


get_addresses_for_facility <- function(facility_id){
  endpoint <- paste0("/facilities/", facility_id, "/facilityaddresses")
  print(endpoint)
  addresses <- get_ridb(endpoint)
  
  # Handle trying to call this function on a facility that doesn't return campsites
  
  if (nrow(addresses) == 0) {
    addresses <- tibble(Note = "No addresses available for this facility.")
  }
  else{
    # get empty columns, for when there is only one address for the facility
    empty_cols <- sapply(addresses, function(x) all(x==""))
    # Only return non empty
    addresses <- addresses[,!empty_cols]
    
  }
  return(addresses)
}



# Wrapper to combine campsites and addresses into details
get_facility_details <- function(facility_id){
  
  campsites <- get_campsites_for_facility(facility_id) 
  addresses <- get_addresses_for_facility(facility_id)
  details <- list(addresses=addresses, campsites = campsites)
  
  return(details)
}
