# Author: Mike Keating
# Suite of functions for querying the RIDB federal recreation API
# Dependencies

library(httr)
library(jsonlite)
suppressWarnings(library(tidyverse))


get_ridb <- function(endpoint, params=list()){
  # Base query which we will modify/call for each specific request
  # Each function will pass it a specific endpoint
  base <- "https://ridb.recreation.gov/api/v1"
  url <- paste0(base, endpoint)
  
  # Get local key
  key <- Sys.getenv("RIDB_API_KEY")
  
  res <- GET(url, query= c(apikey=key, params), accept("application/json"))
  all_data <- content(res, as ="parsed", simplifyVector = TRUE)
  metadata_tibble <- as_tibble(all_data$METADATA$RESULTS)
  recdata_tibble <- as_tibble(all_data$RECDATA)
  return(list(metadata=metadata_tibble, rec_data = all_data$RECDATA))
  
}



get_facilities <- function(state = NULL, activity = NULL, page = 1, per_page = 50){
  # Create a named list of parameters which we will pass to the httr package
  # This is more elegant than the approach used in hw4
  
  params <- list(page = page, limit = per_page)
  
  # These two parameters are optional for the call
  if (!is.null(state)) params$stateAbb <- state
  if (!is.null(activity)) params$activity <- activity
  
  # Call our get_ridb helper function
  get_ridb("/facilities", params)
  
}


