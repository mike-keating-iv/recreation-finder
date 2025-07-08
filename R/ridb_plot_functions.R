# Author: Mike Keating
# Suite of functions for plotting data
# Dependencies


library(ggplot2)
library(dplyr)

create_explore_plot <- function(df, x_var,group_var, plot_type, facet = FALSE) {
  # Generic for now...
  group_var <- if (group_var == "None") NULL else group_var
  p <- ggplot(df, aes_string(x = x_var))
  
  if (plot_type == "Activity Count by X") {
    stopifnot("CountActivities" %in% names(df))
  
    p <- p + aes_string(y = "CountActivities")
    if (!is.null(group_var)) p <- p + aes_string(fill = group_var)
    p <- p + geom_col(position="dodge") + theme_minimal() + 
      theme(axis.text.x = element_text(angle=45, hjust=1)) # Rotate the labels, useful for states with lots of rec areas etc like CA
    
    title <- paste0("Acitivity Count by ", x_var)
    x_lab <- x_var
    y_lab <- "Activity Count"
    p + theme_minimal() + labs(title = title, x=x_lab, y=y_lab)
    if (facet && !is.null(group_var)) {
      p <- p + facet_wrap(as.formula(paste("~", group_var)))
    }
    
    return(p)
    
  } else if (plot_type == "Top Recreation Areas") {
    # Grab the top 5 recreation areas
    top_areas <- df |> filter(!is.na(RecAreaName)) |>
      count(RecAreaName, sort=TRUE) |>
      slice_max(n, n=5)
    # Replace the original plot
    p <- ggplot(top_areas, aes(x = n, y=reorder(RecAreaName,n))) +
      geom_col(position="dodge") + labs(
        title="Top Recreation Areas by Number of Facilities",
        x = "Number of Facilities",
        y = "Recreation Area"
      )
    if (!is.null(group_var)) p <- p + aes_string(fill = group_var)
    title <- "Top Recreation Areas by Number of Facilities"
    x_lab <- "Number of Facilities"
    y_lab = "Recreation Area"
    
    p + theme_minimal() + labs(title = title, x=x_lab, y=y_lab)
    return(p)
    
  } else if (plot_type == "Heatmap: Facility Type vs Recreation Area"){
    
    stopifnot(all(c("RecAreaName", "FacilityTypeDescription") %in% names(df)))
    
    df_counts <- df |> count(RecAreaName, FacilityTypeDescription)
    title <- "Facility Type by Recreation Area"
    x_lab <- "Facility Type"
    y_lab <- "Recreation Area"
    p <- ggplot(df_counts, aes(x = FacilityTypeDescription, y = RecAreaName, fill = n)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightyellow", high = "steelblue") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = title,
        x = x_lab,
        y = y_lab,
        fill = "Count"
      )
    return(p)

}
 
  
}


create_summary_table <- function(df, x_var, y_var = NULL, group_var = NULL) {
  group_var <- if (group_var == "None") NULL else group_var
  var_to_summarize <- y_var %||% x_var
  
  if (!is.null(group_var)) {
    df |>
      group_by(.data[[group_var]]) |>
      summarize(
        Count = n(),
        Mean = mean(as.numeric(.data[[var_to_summarize]]), na.rm = TRUE),
        SD = sd(as.numeric(.data[[var_to_summarize]]), na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    tibble(
      Count = nrow(df),
      Mean = mean(as.numeric(df[[var_to_summarize]]), na.rm = TRUE),
      SD = sd(as.numeric(df[[var_to_summarize]]), na.rm = TRUE)
    )
  }
}


create_facilities_map <- function(df, color_map_group){
  # Create Map of the facilities
  

  

  
  # Filter out locations that are entered as (0,0) which adds points off the coast of Africa
  df <- df |> filter(FacilityLongitude != 0, FacilityLatitude != 0)
  # Color by a user supplied group
  # Default to "Unknown" if values are null
  group_values <- df[[color_map_group]] %||% "Unknown"
  
  # Color Pallete
  pal <- colorFactor("Dark2", domain=unique(group_values))
  
  leaflet(data = df) |>
    addProviderTiles("CartoDB.Positron") |>
    addCircleMarkers(
      lng = ~FacilityLongitude,
      lat = ~FacilityLatitude,
      label = ~FacilityName,
      color = ~pal(group_values),
      popup = ~paste0(
        "<strong>", as.character(FacilityName %||% "Unnamed"), "</strong><br><br>",
        "<strong>Type:</strong> ", as.character(FacilityTypeDescription %||% "Unknown"), "<br>",
        "<strong>Organization:</strong> ", as.character(OrgName %||% "Unknown"), "<br>",
        "<strong>Recreation Area:</strong>", as.character(RecAreaName %||% "Unknown"), "<br>",
        "<strong>Activities:</strong> ", as.character(Activities %||% "None"), "<br>",
        # Add a button that allows us to call the api to fetch details for the specific facility
        "<button onclick='Shiny.setInputValue(\"fetch_facility\", \"", FacilityID, "\", {priority: \"event\"})'>Fetch Details</button>",
        # Add some css to avoid long descriptions from panning the map awkwardly
        # Some of the descriptions are fairly long html, which renders nicely
        "<div style='max-height:200px; overflow-y:auto;'>", # scrollable 
        as.character(FacilityDescription %||% "No description available"),
        "</div>"
      ),
      radius = 4,
      fillOpacity = 0.7,
      stroke = FALSE
    ) |>
    addLegend("bottomright", pal=pal, values=group_values, title = color_map_group, opacity=1)
}

create_contingency_table <- function(df, contingency_choice){
  switch(contingency_choice,
         "orgXtype" = {with(df, table(OrgName, FacilityTypeDescription))},
         "areaXtype" = {with(df, table(RecAreaName, FacilityTypeDescription))},
         "orgXarea" = {with(df, table(OrgName, RecAreaName))})
}