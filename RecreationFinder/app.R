#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
# Author: Mike Keating
library(shiny)
library(DT)
library(bslib)

source("../R/ridb_api_functions.R")

ui <- page_fluid(
  navset_tab(
    nav_panel("About", "About this app"),
    nav_panel("Search", "search"),
    nav_panel("Download",
              sidebarLayout(
                sidebarPanel(
                  textInput("zip", "Enter Zip Code", value = "27701"),
                  numericInput("radius_miles", "Search Radius (miles)", value = 25, min=1,max=200),
                  actionButton("search", "Search Facilities"),
                  hr(),
                  uiOutput("column_selector"),
                  downloadButton("download_data", "Download CSV")
              ),
              mainPanel(dataTableOutput("facility_table")
                        )
              )
              ),
    
    id = "tab"
  )
)

server <- function(input, output){
  
  facilities <- reactiveVal(NULL)
  
  observeEvent(input$search, {
    req(input$zip)
    facs <- get_facilities(zip_code = input$zip, radius_miles = input$radius_miles)
    facilities(facs$rec_data)
  })
  
  # Display the returned data
  output$facility_table <- renderDataTable({
    req(facilities())
    datatable(facilities(), filter = "top")
  })
  
  output$column_selector <- renderUI({
    req(facilities())
    checkboxGroupInput(
      "columns",
      "Select Columns to Keep",
      choices = names(facilities()),
      selected = names(facilities())
    )
  })
  
}

shinyApp(ui = ui, server = server)