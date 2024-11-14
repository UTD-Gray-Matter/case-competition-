library(shiny)
library(dplyr)
library(readr)
library(DT)
library(ggplot2)

# Load the renamed CSV files
Historical_Hurricane_1 <- read_csv("dataset/Historical_Hurricane_1_renamed.csv")
Historical_Hurricane_2 <- read_csv("dataset/Historical_Hurricane_2_renamed.csv")
storm_summary <- read_csv("dataset/storm_summary.csv")
max_wind_summary <- read_csv("dataset/max_wind_summary.csv")
exposures <- read_csv("dataset/exposures_summary.csv")

# UI Definition
ui <- fluidPage(
  titlePanel("Hurricane Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # Tabs for Hurricane 1, Hurricane 2, Storm Counts, Max Wind Speed, Exposures, Policy Characteristics
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
        ),
        tabPanel("Max Wind Speed",
                 selectInput("storm_name_wind", "Select Storm Name (Max Wind)", 
                             choices = unique(max_wind_summary$storm_name), 
                             multiple = TRUE),
                 selectInput("year_wind", "Select Year (Max Wind)", 
                             choices = unique(max_wind_summary$year),
                             selected = min(max_wind_summary$year))
        ),
        # Exposures filters
        tabPanel("Exposures",
                 textInput("location_exposures", "Enter Location (or leave blank for all locations):"),
                 numericInput("past_years_exposures", "View Past 'X' Years:", value = 5, min = 1)
        )
      )
    ),
    
    mainPanel(
      # Main content for Hurricane Data, Boxplots, and Exposures Data
      tabsetPanel(
        tabPanel("Hurricane 1 Data", DT::dataTableOutput("table_h1")),
        tabPanel("Hurricane 2 Data", DT::dataTableOutput("table_h2")),
        tabPanel("Storm Counts", textOutput("storm_summary_output")),
        tabPanel("Max Wind Speed Summary", DT::dataTableOutput("max_wind_speed_table")),
        
        # Exposures-related boxplots
        tabPanel("Premium Boxplot", 
                 plotOutput("premium_plot")
        ),
        tabPanel("Losses - Non Catastrophe Boxplot", 
                 plotOutput("losses_plot")
        ),
        tabPanel("Total Insured Value Boxplot", 
                 plotOutput("tiv_plot")
        ),
        tabPanel("Exposures Data",
                 DT::dataTableOutput("exposures_table")
        )
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
        storm_name %in% input$storm_name_h1,
        SEASON_year >= input$year_range_h1[1],
        SEASON_year <= input$year_range_h1[2]
      )
  })
  
  # Reactive dataset for Hurricane 2 based on user inputs
  filtered_data_h2 <- reactive({
    Historical_Hurricane_2 %>%
      filter(
        storm_name %in% input$storm_name_h2,
        YEAR >= input$year_range_h2[1],
        YEAR <= input$year_range_h2[2]
      )
  })
  
  # Reactive calculation for Storm Counts based on user inputs
  storm_summary_sum <- reactive({
    filtered_data <- storm_summary %>%
      filter(
        SEASON_year == input$storm_year
      )
    
    if (length(input$storm_type) > 0) {
      filtered_data <- filtered_data %>%
        filter(NATURE %in% input$storm_type)
    }
    
    storm_count_sum <- if(nrow(filtered_data) > 0) {
      sum(filtered_data$Storm_Count, na.rm = TRUE)
    } else {
      0
    }
    
    return(storm_count_sum)
  })
  
  # Reactive dataset for Max Wind Speed based on user inputs
  max_wind_speed_data <- reactive({
    max_wind_summary %>%
      filter(
        storm_name %in% input$storm_name_wind,
        year == input$year_wind
      )
  })
  
  # Reactive dataset for Exposures based on user inputs
  filtered_exposures <- reactive({
    current_year <- max(exposures$PolicyYear, na.rm = TRUE)
    past_years_limit <- current_year - input$past_years_exposures
    
    exposures %>%
      filter(
        (Location == input$location_exposures | input$location_exposures == ""),
        PolicyYear >= past_years_limit
      )
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
  
  # Render table for Max Wind Speed Summary
  output$max_wind_speed_table <- DT::renderDataTable({
    max_wind_speed_data()
  })
  
  # Render table for Exposures Data
  output$exposures_table <- DT::renderDataTable({
    filtered_exposures()
  })
  
  # Premium Boxplot
  output$premium_plot <- renderPlot({
    data <- filtered_exposures()
    
    ggplot(data, aes(x = factor(Location), y = Premium)) +
      geom_boxplot() +
      labs(title = "Premium Distribution by Location", x = "Location", y = "Premium") +
      theme_minimal()
  })
  
  # Losses - Non Catastrophe Boxplot
  output$losses_plot <- renderPlot({
    data <- filtered_exposures()
    
    ggplot(data, aes(x = factor(Location), y = `Losses - Non Catastrophe`)) +
      geom_boxplot() +
      labs(title = "Losses - Non Catastrophe by Location", x = "Location", y = "Losses - Non Catastrophe") +
      theme_minimal()
  })
  
  # Total Insured Value Boxplot
  output$tiv_plot <- renderPlot({
    data <- filtered_exposures()
    
    ggplot(data, aes(x = factor(Location), y = `Total Insured Value`)) +
      geom_boxplot() +
      labs(title = "Total Insured Value by Location", x = "Location", y = "Total Insured Value") +
      theme_minimal()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)