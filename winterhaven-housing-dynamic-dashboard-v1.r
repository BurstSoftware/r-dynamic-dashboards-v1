library(shiny)
library(ggplot2)
library(leaflet)
library(DT)
library(dplyr)
library(tidyr)

# Default dataset
default_data <- data.frame(
  City = c("Lakeland", "Brandon", "Winter Haven", "Plant City", "Winter Garden", 
           "Bartow", "Auburndale", "Haines City", "Lake Wales", "Dundee", 
           "Lake Alfred", "Polk City", "Eagle Lake"),
  State = rep("Florida", 13),
  ZIP_Code = c(33801, 33510, 33880, 33563, 34787, 33830, 33823, 33844, 
               33853, 33838, 33850, 33868, 33839),
  Population = c(115425, 115194, 52710, 39764, 46327, 20714, 17295, 27056, 
                 16361, 5235, 6374, 2580, 3044),
  Number_of_Homes = c(42500, 40700, 19300, 14200, 16900, 7900, 6200, 9400, 
                      6100, 2000, 2400, 950, 1150),
  Market_Tier = c("Secondary", "Secondary", "Secondary", "Secondary", "Primary", 
                  "Emerging", "Emerging", "Secondary", "Emerging", "Rural", 
                  "Emerging", "Rural", "Rural"),
  Avg_Home_Value = c(275000, 350000, 295000, 295000, 475000, 265000, 280000, 
                     295000, 260000, 230000, 245000, 225000, 235000),
  Avg_Home_Price = c(285000, 365000, 305000, 310000, 490000, 270000, 285000, 
                     300000, 265000, 235000, 250000, 230000, 240000),
  Annual_Home_Sales = c(2200, 2500, 1400, 1200, 2500, 750, 680, 1100, 620, 
                        150, 300, 100, 120),
  Number_of_Current_Contractor = c(2, 1, 3, 2, 3, 2, 2, 2, 1, 1, 1, 1, 1),
  stringsAsFactors = FALSE
)

# UI
ui <- fluidPage(
  titlePanel("Housing Market Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h4("Data Input"),
      fileInput("file", "Upload CSV File", accept = ".csv"),
      p("Or use the default dataset provided."),
      hr(),
      h4("Filters"),
      selectInput("market_tier", "Market Tier", 
                  choices = c("All", unique(default_data$Market_Tier))),
      sliderInput("population", "Population Range",
                  min = min(default_data$Population), 
                  max = max(default_data$Population), 
                  value = c(min(default_data$Population), max(default_data$Population))),
      hr(),
      h4("Plot Options"),
      selectInput("plot_type", "Plot Type", 
                  choices = c("Scatter: Home Value vs Price", 
                              "Bar: Annual Home Sales by City", 
                              "Box: Home Value by Market Tier")),
      downloadButton("download_data", "Download Filtered Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DTOutput("table")),
        tabPanel("Visualizations", plotOutput("plot")),
        tabPanel("Map", leafletOutput("map"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data
  data <- reactive({
    if (is.null(input$file)) {
      df <- default_data
    } else {
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      # Validate columns
      required_cols <- c("City", "State", "ZIP_Code", "Population", 
                         "Number_of_Homes", "Market_Tier", "Avg_Home_Value", 
                         "Avg_Home_Price", "Annual_Home_Sales", 
                         "Number_of_Current_Contractor")
      if (!all(required_cols %in% colnames(df))) {
        stop("Uploaded file must contain all required columns: ", 
             paste(required_cols, collapse = ", "))
      }
    }
    
    # Filter data
    df <- df %>%
      filter(Population >= input$population[1], Population <= input$population[2])
    
    if (input$market_tier != "All") {
      df <- df %>% filter(Market_Tier == input$market_tier)
    }
    
    df
  })
  
  # Update slider max/min based on data
  observe({
    updateSliderInput(session, "population",
                     min = min(default_data$Population),
                     max = max(default_data$Population),
                     value = c(min(default_data$Population), max(default_data$Population)))
  })
  
  # Data table
  output$table <- renderDT({
    datatable(data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Plot
  output$plot <- renderPlot({
    df <- data()
    if (input$plot_type == "Scatter: Home Value vs Price") {
      ggplot(df, aes(x = Avg_Home_Value, y = Avg_Home_Price, color = Market_Tier)) +
        geom_point(size = 3) +
        labs(title = "Average Home Value vs Price", x = "Avg Home Value", y = "Avg Home Price") +
        theme_minimal()
    } else if (input$plot_type == "Bar: Annual Home Sales by City") {
      ggplot(df, aes(x = reorder(City, -Annual_Home_Sales), y = Annual_Home_Sales, fill = Market_Tier)) +
        geom_bar(stat = "identity") +
        labs(title = "Annual Home Sales by City", x = "City", y = "Annual Home Sales") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$plot_type == "Box: Home Value by Market Tier") {
      ggplot(df, aes(x = Market_Tier, y = Avg_Home_Value, fill = Market_Tier)) +
        geom_boxplot() +
        labs(title = "Home Value Distribution by Market Tier", x = "Market Tier", y = "Avg Home Value") +
        theme_minimal()
    }
  })
  
  # Map (using ZIP codes for approximate coordinates)
  output$map <- renderLeaflet({
    df <- data()
    
    # Approximate coordinates for ZIP codes (for demo purposes; replace with real geocoding if needed)
    coords <- data.frame(
      ZIP_Code = c(33801, 33510, 33880, 33563, 34787, 33830, 33823, 33844, 
                   33853, 33838, 33850, 33868, 33839),
      lat = c(28.0395, 27.9378, 27.9934, 28.0186, 28.5653, 27.8978, 28.0875, 
              28.1092, 27.8653, 28.0806, 28.1121, 28.2353, 27.9014),
      lng = c(-81.9498, -82.2859, -81.7279, -82.1311, -81.5862, -81.8009, 
              -81.7187, -81.6207, -81.5898, -81.6117, -81.6778, -81.8237, -81.7062)
    )
    
    df <- left_join(df, coords, by = "ZIP_Code")
    
    leaflet(df) %>%
      addTiles() %>%
      addCircles(
        lng = ~lng, lat = ~lat, 
        radius = ~Population / 10, 
        popup = ~paste(City, "<br>Population: ", Population, 
                       "<br>Avg Home Value: $", Avg_Home_Value),
        color = ~case_when(
          Market_Tier == "Primary" ~ "blue",
          Market_Tier == "Secondary" ~ "green",
          Market_Tier == "Emerging" ~ "orange",
          Market_Tier == "Rural" ~ "red"
        )
      )
  })
  
  # Download filtered data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_housing_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui, server)
