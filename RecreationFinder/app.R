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
library(leaflet) # For mapping

# Import our custom functions
source("../R/ridb_api_functions.R")
source("../R/ridb_plot_functions.R")

ui <- page_fluid(
  navset_tab(
    nav_panel("About", "RIDB is a part of the Recreation One Stop Program (R1S) provides a single source of recreational oppurtunities nationwide."),
    nav_panel("Search Facilities",
              sidebarLayout(
                sidebarPanel(
                  h3("Search for Recreation Facilities"),
                  h5("Optionally Select Activities"),
                  selectInput("activity", "Select Activity", choices = NULL, selected = NULL, multiple = TRUE),
                  hr(),
                  h5("Search by State or Zip Code"),
                  # Allow drop down of all states using built in sate.abb function
                  selectInput("state", "Enter State", choices = state.abb, multiple = FALSE),
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
    nav_panel("Explore",
              fluidPage(
                selectInput("explore_mode", "Choose View",
                            choices = c("Plot" = "plot", "Map" = "map"),
                            selected = "map"),
                conditionalPanel(
                  condition = "input.explore_mode == 'plot'",
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("x_var", "X Variable", choices = NULL),
                      selectInput("y_var", "Y Variable", choices = NULL),
                      selectInput("group_var", "Group / Fill Variable", choices = NULL),
                      selectInput("plot_type", "Plot Type",
                                  choices = c("Scatterplot", "Boxplot", "Bar Plot")),
                      checkboxInput("add_facet", "Facet by Group?", value = FALSE)
                    ),
                    mainPanel(
                      plotOutput("explore_plot"),
                      hr(),
                      tableOutput("summary_table")
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.explore_mode == 'map'",
                  leafletOutput("facility_map", height = 600),
                  hr(),
                  #tableOutput()
                )
              )
    ),
    
    id = "tab"
  )
)

server <- function(input, output){
  
  facilities <- reactiveVal(NULL)
  activities <- reactiveVal(NULL)
  
  ### SEARCH TAB
  # Search for and download data
  # Populate the dropdown for activities on startup
  observe({
    act <- get_activities()
    activities(act)
    
    updateSelectInput(inputId = "activity",
                      choices = setNames(act$ActivityID, act$ActivityName))
  })
  
  observeEvent(input$search_by_zip, {
    req(input$zip)
    facs <- get_facilities(zip_code = input$zip, radius_miles = input$radius_miles)
    facilities(facs)
  })
  
  observeEvent(input$search_by_state,{
    req(input$state)
    facs <- get_facilities(state=input$state, activity = input$activity)
    facilities(facs)
  })
  
  # Display the returned data
  #https://stackoverflow.com/questions/71083229/with-dt-datatable-is-it-possible-to-have-a-column-that-holds-very-long-case-n
  output$facility_table <- renderDataTable({
    req(facilities())
    selected_cols <- input$columns %||% names(facilities())
    # Don't display the columns that contain objects
    # We want to keep them in the actual tibble though
    selected_cols <- setdiff(selected_cols, c("ACTIVITY", "ORGANIZATION"))
    
    # Show a note if our api call returns nothing
    if (nrow(facilities()) == 0) {
      return(datatable(
        tibble(Note = "No facilities found for the selected criteria."),
        # Hide all the pages etc since we are just showing a note
        options = list(dom = 't')  
      ))
    }
    
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
  
  # EXPLORE TAB
  observeEvent(facilities(), {
    updateSelectInput(inputId = "x_var", choices = names(facilities()))
    updateSelectInput(inputId = "y_var", choices = names(facilities()))
    updateSelectInput(inputId = "group_var", choices = c("None", names(facilities())))
  })
  
  output$explore_plot <- renderPlot({
    req(facilities(), input$x_var, input$plot_type)
    create_explore_plot(
      df = facilities(),
      x_var = input$x_var,
      y_var = input$y_var,
      group_var = input$group_var,
      plot_type = input$plot_type,
      facet = input$add_facet
    )
  })
  
  output$summary_table <- renderTable({
    req(facilities(), input$x_var)
    create_summary_table(
      df = facilities(),
      x_var = input$x_var,
      y_var = input$y_var,
      group_var = input$group_var
    )
  })
  
  output$facility_map <- renderLeaflet({
    req(facilities())
  
    create_facilities_map(facilities())
    
  })
  
  
}

shinyApp(ui = ui, server = server)