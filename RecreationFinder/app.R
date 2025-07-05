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
                  textInput("state", "Enter State", value = "TN"),
                  actionButton("search_by_state", "Search Facilities by State"),
                  hr(),
                  textInput("zip", "Enter Zip Code", value = "37738"),
                  numericInput("radius_miles", "Search Radius (miles)", value = 25, min=1,max=50),
                  actionButton("search_by_zip", "Search Facilities by Zip"),
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
  
  observeEvent(input$search_by_zip, {
    req(input$zip)
    facs <- get_facilities(zip_code = input$zip, radius_miles = input$radius_miles)
    facilities(facs)
  })
  
  observeEvent(input$search_by_state,{
    req(input$state)
    facs <- get_facilities(state=input$state)
    facilities(facs)
  })
  
  # Display the returned data
  #https://stackoverflow.com/questions/71083229/with-dt-datatable-is-it-possible-to-have-a-column-that-holds-very-long-case-n
  output$facility_table <- renderDataTable({
    req(facilities())
    selected_cols <- input$columns %||% names(facilities())
    datatable(
      facilities()[,selected_cols, drop = FALSE],
      filter = "top",
      options = list(
        scrollX = TRUE,
        columnDefs = list(
          list(
            # Apply the ellipsis truncation to every selected column
            targets = seq_along(selected_cols)-1,  
            # Here we are going to truncate long text to 50 characters
            render = JS("$.fn.dataTable.render.ellipsis(50, false)")  
          )
        )
      ),
      plugins = "ellipsis"
    )
  })
  
  # Select columns
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