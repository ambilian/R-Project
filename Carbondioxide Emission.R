# Loading libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(sf)
library(plotly)

# Reading the Dataset
co2_data <- read.csv("greenhouse-gas-emissions-by-region-industry-and-household-year-ended-2023.csv", stringsAsFactors = FALSE)

co2_data <- co2_data %>% filter(Gas == "Carbon dioxide equivalents")
# Remove rows where Anzsic_descriptor is 'Total' or 'Total all industries'
co2_data <- co2_data %>% filter(!(Anzsic_descriptor %in% c("Total", "Total all industries")))
co2_data <- co2_data %>% distinct()  # Remove duplicates
missing_values <- colSums(is.na(co2_data))

region_mapping <- c(
  "Auckland" = "Auckland Region",
  "Bay of Plenty" = "Bay of Plenty Region",
  "Canterbury" = "Canterbury Region",
  "Gisborne" = "Gisborne Region",
  "Hawke's Bay" = "Hawke's Bay Region",
  "Manawatu-Whanganui" = "Manawatu-Whanganui Region",
  "Marlborough" = "Marlborough Region",
  "Northland" = "Northland Region",
  "Otago" = "Otago Region",
  "Southland" = "Southland Region",
  "Taranaki" = "Taranaki Region",
  "Tasman/Nelson" = "Tasman-Nelson Region",
  "Waikato" = "Waikato Region",
  "Wellington" = "Wellington Region",
  "West Coast" = "West Coast Region"
)

# Shape files reading
nz_shapefile$Region <- recode(nz_shapefile$REGC2025_1, !!!region_mapping)
nz_shapefile$Region <- gsub(" Region", "", nz_shapefile$Region)
co2_data$Region <- gsub("Manawatu-Whanganui", "Manawatū-Whanganui", co2_data$Region)

# Define UI and server for Shiny App
ui <- fluidPage(
  titlePanel("CO2 Gas Emissions by Region (Clickable Map)"),
  
  fluidRow(
    column(6, plotOutput("map", click = "map_click")),
    column(6, plotlyOutput("bar_graph", height = "400px"))  # Height added for bar chart
  ),
  
  fluidRow(
    column(12, uiOutput("region_summary_text"))
  ),
  
  div(style = "height: 20px;"),
  
  fluidRow(
    column(12, plotlyOutput("industry_pie", height = "400px"))  
  ),
)

server <- function(input, output, session) {
  
  selected_region <- reactiveVal(NULL)
  selected_color <- reactiveVal(NULL) 
  clicked_year <- reactiveVal(NULL)
  
  # Precompute summary for all regions
  co2_summary <- co2_data %>% 
    mutate(Region = gsub(" Region", "", Region)) %>% 
    group_by(Region) %>% 
    summarise(
      min_year = Year[which.min(Data_value)],
      min_value = min(Data_value),
      max_year = Year[which.max(Data_value)],
      max_value = max(Data_value)
    ) %>% 
    mutate(text_message = paste0(
      "Region: ", Region, 
      " - Lowest CO2 Emission: ", min_value, " in ", min_year, 
      " | Highest CO2 Emission: ", max_value, " in ", max_year
    ))
  
  # Create a color scale based on the total CO2 emissions
  region_colors <- co2_data %>%
    group_by(Region) %>%
    summarise(total_emissions = sum(Data_value, na.rm = TRUE)) %>%
    mutate(color = scales::col_numeric(palette = brewer.pal(9, "YlGnBu"), domain = range(total_emissions))(total_emissions))
  
  # Map output
  output$map <- renderPlot({
    ggplot() +
      geom_sf(data = nz_shapefile, aes(fill = Region), color = "white") + 
      theme_minimal() +
      scale_fill_manual(values = setNames(region_colors$color, region_colors$Region)) + 
      labs(title = "Click on the New Zealand Map") +
      coord_sf()
  })
  
  # Update selected region when clicked on the map
  observeEvent(input$map_click, {
    req(input$map_click)
    
    clicked_point <- sf::st_sfc(
      sf::st_point(c(input$map_click$x, input$map_click$y)),
      crs = sf::st_crs(nz_shapefile)
    ) %>% sf::st_sf(geometry = .)
    
    result <- sf::st_intersects(clicked_point, nz_shapefile)
    
    # Only proceed if result is valid and non-empty
    if (length(result) > 0 && length(result[[1]]) > 0) {
      matched_index <- result[[1]][1]
      region_name <- gsub(" Region", "", nz_shapefile$Region[matched_index])
      selected_region(region_name)
      
      region_color <- region_colors %>% 
        dplyr::filter(Region == region_name) %>% 
        dplyr::pull(color)
      
      selected_color(region_color)
      clicked_year(NULL)
    } else {
      message("No region matched the clicked point.")
    }
  })
  
  # Get data for the selected region
  selected_data <- reactive({
    req(selected_region())
    co2_data %>% 
      mutate(Region = gsub(" Region", "", Region)) %>% 
      filter(Region == selected_region()) %>% 
      group_by(Year) %>% 
      summarise(total_emissions = sum(Data_value, na.rm = TRUE))
  })
  
  # Render bar graph
  output$bar_graph <- renderPlotly({
    data <- selected_data()
    region_color <- selected_color()  
    if (is.null(region_color)) {
      region_color <- "gray"
    }
    
    if (nrow(data) == 0) {
      plot_ly() %>% layout(title = "No Data Available for Selected Region")
    } else {
      plot_ly(
        data,
        x = ~Year,
        y = ~total_emissions,
        type = 'bar',
        text = ~total_emissions,
        textposition = 'outside',
        marker = list(color = region_color),
        source = "bar"
      ) %>% layout(
        title = paste("CO2 Emissions in", selected_region()),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Total CO2 Emissions")
      ) %>%
        config(displayModeBar = FALSE)
    }
  })
  
  # Display region summary
  output$region_summary_text <- renderUI({
    req(selected_region())
    message_row <- co2_summary %>% filter(Region == selected_region())
    if (nrow(message_row) > 0) {
      HTML(paste(
        "<div style='font-size:18px; text-align:right;'>", 
        message_row$text_message, 
        "</div>"
      ))
    } else {
      HTML("<div style='font-size:18px; text-align:right;'>No summary available for this region.</div>")
    }
  })
  
  industry_data <- reactive({
    req(selected_region())
    co2_data %>%
      mutate(Region = gsub(" Region", "", Region)) %>%
      filter(Region == selected_region()) %>%
      group_by(Anzsic_descriptor) %>%
      summarise(total_emissions = sum(Data_value, na.rm = TRUE)) %>%
      filter(total_emissions > 0)
  })
  
  output$industry_pie <- renderPlotly({
    data <- industry_data()
    if (nrow(data) == 0) {
      plot_ly() %>% layout(title = "No Industry Data Available")
    } else {
      plot_ly(data,
              labels = ~Anzsic_descriptor,
              values = ~total_emissions,
              type = 'pie') %>%
        layout(title = paste("Total CO2 Emissions from 2007 to 2023 by Industry in", selected_region()),
               title_x = 0,  
               title_xanchor = "left",  
               title_xref = "paper")
    }
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
