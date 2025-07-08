# Recreation Finder 

An interactive R Shiny application that allows users to explore U.S. recreation facilities via the [Recreation Information Database (RIDB)](https://ridb.recreation.gov/) API.

Developed by **Mike Keating** as part of the ST558 Data Science course project.

---

## Features

- **Facility Search**:
  - Query by **state**, **ZIP code + radius**, and/or **activity type**
  - Results include metadata (coordinates, organization, facility type, description, activities, etc.)
  - Downloadable filtered table with optional column selection.

- **Interactive Map**:
  - Facility markers color-coded by **organization**, **recreation area**, or **facility type**
  - Scrollable popups with descriptions and embedded facility metadata
  - Button to fetch and display full **facility details** and **campsites** on another tab.

- **Data Exploration**:
  - Create customizable plots of facility data
    - Bar plots, boxplots, dot plots, heatmaps, etc.
    - Grouped, colored, and faceted visualizations
  - Summary statistics and predefined **contingency tables** (e.g., Org × Facility Type)

- **Facility Detail View**:
  - Dedicated tab to display:
    - Information on specific campsites in a facility, if available.
    - Address information for the facility.

---

## RIDB Queries

This app queries the following endpoints in the RIDB API:
- facilities (by state, zip code + radius, or activity)
- activities (all available, this is to populate a dropdown menu)
- campsites (by FacilityID)
- addresses (by FacilityID)

---

##  R Packages

- **Frontend Stuff**: `shiny`, `bslib`, `leaflet`
- **Data Wrangling**: `httr`, `jsonlite`, `tidyverse` 

---

## Running the App
First install dependencies:
'''r
install.packages(c("shiny", "DT", "bslib", "leaflet", "httr", "jsonlite", "tidyverse", "zipcodeR"))

shiny::runGitHub(
  repo = "st558-project2",        
  username = "mike-keating-iv",
  subdir = "RecreationFinder",
  ref = "main"              
)


