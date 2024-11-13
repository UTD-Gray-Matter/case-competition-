library(shiny)
library(dplyr)
library(readr)
library(DT)

# Load the renamed CSV files
Historical_Hurricane_1 <- read_csv("dataset/Historical_Hurricane_1_renamed.csv")
Historical_Hurricane_2 <- read_csv("dataset/Historical_Hurricane_2_renamed.csv")
storm_summary <- read_csv("dataset/storm_summary.csv")

# UI Definition
ui <- fluidPage(
  titlePanel("Hurricane Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # Tabs to switch between Hurricane 1, Hurricane 2, and Storm Counts
      tabsetPanel(
        tabPanel("Hurricane 1", 
                 selectInput("storm_name_h1", "Select Storm Name (H1)", 
                             choices = unique(Historical_Hurricane_1$storm_name), 
                             multiple = TRUE),
                 sliderInput("year_range_h1", "Select Year Range (H1)", 
                             min = min(Historical_Hurricane_1$SEASON_year), 
                             max = max(Historical_Hurricane_1$SEASON_year), 
                             value = c(min(Historical_Hurricane_1$SEASON_year), max(Historical_Hurricane_1$SEASON_year)), 
                             step = 1)
        ),
        tabPanel("Hurricane 2", 
                 selectInput("storm_name_h2", "Select Storm Name (H2)", 
                             choices = unique(Historical_Hurricane_2$storm_name), 
                             multiple = TRUE),
                 sliderInput("year_range_h2", "Select Year Range (H2)", 
                             min = min(Historical_Hurricane_2$YEAR), 
                             max = max(Historical_Hurricane_2$YEAR), 
                             value = c(min(Historical_Hurricane_2$YEAR), max(Historical_Hurricane_2$YEAR)), 
                             step = 1)
        ),
        tabPanel("Storm Counts",
                 selectInput("storm_type", "Select Storm Type", 
                             choices = unique(storm_summary$NATURE), 
                             multiple = TRUE),
                 selectInput("storm_year", "Select Year", 
                             choices = unique(storm_summary$SEASON_year), 
                             selected = min(storm_summary$SEASON_year))
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Hurricane 1 Data", DT::dataTableOutput("table_h1")),
        tabPanel("Hurricane 2 Data", DT::dataTableOutput("table_h2")),
        tabPanel("Storm Counts", textOutput("storm_summary_output"))
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Reactive dataset for Hurricane 1 based on user inputs
  filtered_data_h1 <- reactive({
    Historical_Hurricane_1 %>%
      filter(
        SEASON_year >= input$year_range_h1[1],
        SEASON_year <= input$year_range_h1[2],
        # If storm names are selected, filter by them, otherwise don't filter
        (is.null(input$storm_name_h1) || storm_name %in% input$storm_name_h1)
      )
  })
  
  # Reactive dataset for Hurricane 2 based on user inputs
  filtered_data_h2 <- reactive({
    Historical_Hurricane_2 %>%
      filter(
        YEAR >= input$year_range_h2[1],
        YEAR <= input$year_range_h2[2],
        # If storm names are selected, filter by them, otherwise don't filter
        (is.null(input$storm_name_h2) || storm_name %in% input$storm_name_h2)
      )
  })
  
  # Reactive calculation for Storm Counts based on user inputs
  storm_summary_sum <- reactive({
    # Filter by selected storm types and selected year
    filtered_data <- storm_summary %>%
      filter(
        SEASON_year == input$storm_year
      )
    
    # If storm types are selected, filter by them
    if (length(input$storm_type) > 0) {
      filtered_data <- filtered_data %>%
        filter(NATURE %in% input$storm_type)
    }
    
    # Check if any rows match the filter, otherwise return 0
    storm_count_sum <- if(nrow(filtered_data) > 0) {
      sum(filtered_data$Storm_Count, na.rm = TRUE)
    } else {
      0
    }
    
    return(storm_count_sum)
  })
  
  # Render the storm count sum in the main panel
  output$storm_summary_output <- renderText({
    storm_count_sum <- storm_summary_sum()
    paste("Total Storm Count for selected type and year: ", storm_count_sum)
  })
  
  # Render table for Hurricane 1
  output$table_h1 <- DT::renderDataTable({
    filtered_data_h1()
  })
  
  # Render table for Hurricane 2
  output$table_h2 <- DT::renderDataTable({
    filtered_data_h2()
  })
}

# Run the app
shinyApp(ui = ui, server = server)