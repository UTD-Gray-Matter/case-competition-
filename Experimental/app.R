library(shiny)
library(dplyr)
library(readr)
library(DT)  # For rendering data tables

# Load the renamed CSV files
Historical_Hurricane_1 <- read_csv("dataset/Historical_Hurricane_1_renamed.csv")
Historical_Hurricane_2 <- read_csv("dataset/Historical_Hurricane_2_renamed.csv")

# UI Definition
ui <- fluidPage(
  titlePanel("Hurricane Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # Tabs to switch between Hurricane 1 and Hurricane 2
      tabsetPanel(
        tabPanel("Hurricane 1", 
                 selectInput("storm_name_h1", "Select Storm Name (H1)", choices = unique(Historical_Hurricane_1$storm_name), multiple = TRUE),
                 sliderInput("year_range_h1", "Select Year Range (H1)", 
                             min = min(Historical_Hurricane_1$SEASON_year), 
                             max = max(Historical_Hurricane_1$SEASON_year), 
                             value = c(min(Historical_Hurricane_1$SEASON_year), max(Historical_Hurricane_1$SEASON_year)), 
                             step = 1)
        ),
        tabPanel("Hurricane 2", 
                 selectInput("storm_name_h2", "Select Storm Name (H2)", choices = unique(Historical_Hurricane_2$storm_name), multiple = TRUE),
                 sliderInput("year_range_h2", "Select Year Range (H2)", 
                             min = min(Historical_Hurricane_2$YEAR), 
                             max = max(Historical_Hurricane_2$YEAR), 
                             value = c(min(Historical_Hurricane_2$YEAR), max(Historical_Hurricane_2$YEAR)), 
                             step = 1)
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Hurricane 1 Data", DT::dataTableOutput("table_h1")),
        tabPanel("Hurricane 2 Data", DT::dataTableOutput("table_h2"))
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Reactive dataset for Hurricane 1 based on user inputs
  filtered_data_h1 <- reactive({
    Historical_Hurricane_1 %>%
      filter(storm_name %in% input$storm_name_h1,
             SEASON_year >= input$year_range_h1[1],
             SEASON_year <= input$year_range_h1[2])
  })
  
  # Reactive dataset for Hurricane 2 based on user inputs
  filtered_data_h2 <- reactive({
    Historical_Hurricane_2 %>%
      filter(storm_name %in% input$storm_name_h2,
             YEAR >= input$year_range_h2[1],
             YEAR <= input$year_range_h2[2])
  })
  
  # Render tables for Hurricane 1
  output$table_h1 <- DT::renderDataTable({
    filtered_data_h1()
  })
  
  # Render tables for Hurricane 2
  output$table_h2 <- DT::renderDataTable({
    filtered_data_h2()
  })
}

# Run the app
shinyApp(ui = ui, server = server)