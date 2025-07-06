# Author: Mike Keating
# Suite of functions for plotting data
# Dependencies


library(ggplot2)
library(dplyr)

create_explore_plot <- function(df, x_var, y_var, group_var, plot_type, facet = FALSE) {
  # Generic for now...
  group_var <- if (group_var == "None") NULL else group_var
  p <- ggplot(df, aes_string(x = x_var))
  
  if (plot_type == "Scatterplot") {
    stopifnot(!is.null(y_var))
    p <- p + aes_string(y = y_var)
    if (!is.null(group_var)) p <- p + aes_string(color = group_var)
    p <- p + geom_point()
  } else if (plot_type == "Boxplot") {
    stopifnot(!is.null(y_var))
    p <- p + aes_string(y = y_var)
    if (!is.null(group_var)) p <- p + aes_string(fill = group_var)
    p <- p + geom_boxplot()
  } else if (plot_type == "Bar Plot") {
    if (!is.null(group_var)) p <- p + aes_string(fill = group_var)
    p <- p + geom_bar()
  }
  
  if (facet && !is.null(group_var)) {
    p <- p + facet_wrap(as.formula(paste("~", group_var)))
  }
  
  p + theme_minimal() + labs(title = "Data Explorer Plot")
}


create_summary_table <- function(df, x_var, y_var = NULL, group_var = NULL) {
  group_var <- if (group_var == "None") NULL else group_var
  var_to_summarize <- y_var %||% x_var
  
  if (!is.null(group_var)) {
    df %>%
      group_by(.data[[group_var]]) %>%
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


create_facilities_map <- function(df){
  # Create Map of the facilities
  
  # Filter out locations that are entered as (0,0) which adds points off the coast of Africa
  df <- df |> filter(FacilityLongitude != 0, FacilityLatitude != 0)
  
  leaflet(data = df) |>
    addProviderTiles("CartoDB.Positron") |>
    addCircleMarkers(
      lng = ~FacilityLongitude,
      lat = ~FacilityLatitude,
      label = ~FacilityName,
      popup = ~paste0(
        "<strong>", as.character(FacilityName %||% "Unnamed"), "</strong><br><br>",
        "<strong>Type:</strong> ", as.character(FacilityTypeDescription %||% "Unknown"), "<br>",
        "<strong>Organization:</strong> ", as.character(OrgName %||% "Unknown"), "<br>",
        "<strong>Activities:</strong> ", as.character(Activities %||% "None"), "<br>",
        # Add some css to avoid long descriptions from panning the map awkwardly
        # Some of the descriptions are fairly long html, which renders nicely
        "<div style='max-height:200px; overflow-y:auto;'>", # scrollable 
        as.character(FacilityDescription %||% "No description available"),
        "</div>"
      ),
      radius = 4,
      fillOpacity = 0.7
    )
}