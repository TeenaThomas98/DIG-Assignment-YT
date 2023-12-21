# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Load your dataset
your_data <- read.csv("DIG.csv", stringsAsFactors = FALSE)

# Mapping numeric labels to descriptive ones for Sex variable
your_data$SEX <- ifelse(your_data$SEX == 1, "Male", "Female")

# UI
ui <- fluidPage(
  titlePanel("Heart Failure Treatment Dataset Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("race_filter", "Filter by Race",
                  choices = c("All", unique(your_data$RACE))),
      selectInput("sex_filter", "Filter by Sex",
                  choices = c("All", "Male", "Female")),
      sliderInput("age_filter", "Filter by Age",
                  min = min(your_data$AGE), max = max(your_data$AGE), value = c(min(your_data$AGE), max(your_data$AGE))),
      # Add more interactive filter options based on your dataset
      
      # You can add additional input widgets as needed for health indicators, outcomes, etc.
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Summary", 
                 verbatimTextOutput("data_summary")),
        
        tabPanel("Descriptive Statistics", 
                 tableOutput("stats_table")),
        
        tabPanel("Patient Characteristics",
                 plotOutput("demographics_plot"),
                 plotOutput("health_indicators_plot")
        ),
        
        tabPanel("Correlation Analysis",
                 plotlyOutput("correlation_plot")),
        
        tabPanel("Relationship Analysis",
                 plotOutput("relationship_plot")
        ),
        
        tabPanel("Survival Analysis",
                 plotOutput("km_survival_curves"),
                 plotOutput("time_to_event_plots")
        ),
        
        tabPanel("Comparative Analysis",
                 plotOutput("comparative_health_indicators"),
                 plotOutput("adverse_events_comparison")
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Filtered data based on user inputs
  filtered_data <- reactive({
    filtered <- your_data
    
    if (input$race_filter != "All") {
      filtered <- filtered %>% filter(RACE == input$race_filter)
    }
    if (input$sex_filter != "All") {
      filtered <- filtered %>% filter(SEX == input$sex_filter)
    }
    filtered <- filtered %>%
      filter(AGE >= input$age_filter[1], AGE <= input$age_filter[2])
    # Add more filters based on your dataset
    
    return(filtered)
  })
  
  output$data_summary <- renderPrint({
    paste("Number of Rows:", nrow(filtered_data()))
    # Add more summary info as needed
  })
  
  output$stats_table <- renderTable({
    # Basic descriptive statistics for filtered data
    summary_stats <- filtered_data() %>%
      select(TRTMT, AGE, EJF_PER, CREAT, DIGUSE, DIURETK) %>%
      summarise_all(list(mean = mean, sd = sd, min = min, max = max))
    summary_stats
  })
  
  output$demographics_plot <- renderPlot({
    # Bar plot for filtered patient demographics
    ggplot(filtered_data(), aes(x = factor(RACE))) + 
      geom_bar(aes(fill = SEX), position = "dodge") +
      labs(title = "Patient Demographics", x = "Race", y = "Count", fill = "Sex")
    # Adjust aesthetics and variables as needed
  })
  
  output$health_indicators_plot <- renderPlot({
    # Boxplot or histogram for health indicators of filtered data segmented by treatment group
    ggplot(filtered_data(), aes(x = factor(TRTMT), y = EJF_PER)) +
      geom_boxplot() +
      labs(title = "Ejection Fraction by Treatment Group", x = "Treatment", y = "Ejection Fraction")
    # Adjust aesthetics and variables as needed
  })
  
  output$correlation_plot <- renderPlotly({
    # Calculate correlation matrix for filtered data
    numeric_data <- filtered_data() %>% select_if(is.numeric) %>% select(-ID, -TRTMT)
    correlation_matrix <- cor(numeric_data)
    plot_ly(z = ~correlation_matrix, type = "heatmap")
  })
  
  output$relationship_plot <- renderPlot({
    # Scatterplot or boxplot to highlight relationships between variables of interest in filtered data
    ggplot(filtered_data(), aes(x = EJF_PER, y = CVD, color = factor(TRTMT))) +
      geom_point() +
      labs(title = "Ejection Fraction vs Cardiovascular Mortality", x = "Ejection Fraction", y = "Cardiovascular Mortality", color = "Treatment") +
      theme_minimal()
    # Adjust aesthetics and variables as needed
  })
  
  output$km_survival_curves <- renderPlot({
    # Include survival analysis code for filtered data here
  })
  
  output$time_to_event_plots <- renderPlot({
    # Include time-to-event analysis code for filtered data here
  })
  
  output$comparative_health_indicators <- renderPlot({
    # Boxplots or histograms for health indicators between treatment groups in filtered data
    ggplot(filtered_data(), aes(x = factor(TRTMT), y = EJF_PER)) +
      geom_boxplot() +
      labs(title = "Comparative Health Indicators between Treatment Groups", x = "Treatment", y = "Ejection Fraction")
    # Adjust aesthetics and variables as needed
  })
  
  output$adverse_events_comparison <- renderPlot({
    # Grouped bar charts to compare incidence of adverse events or hospitalizations between treatment groups in filtered data
    filtered_data() %>%
      group_by(TRTMT) %>%
      summarise(adverse_event_count = sum(ADVERSE_EVENT_COLUMN)) %>%
      ggplot(aes(x = factor(TRTMT), y = adverse_event_count, fill = factor(TRTMT))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Comparison of Adverse Events between Treatment Groups", x = "Treatment", y = "Count", fill = "Treatment")
    # Adjust variables and column names accordingly
  })
}

# Run the app
shinyApp(ui = ui, server = server)

