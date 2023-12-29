# Load necessary libraries if not already loaded
if (!require("janitor")) install.packages("janitor")
if (!require("lubridate")) install.packages("lubridate")
if (!require("shiny")) install.packages("shiny")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("palmerpenguins")) install.packages("palmerpenguins")
if (!require("shinydashboard")) install.packages("shinydashboard")

# Load necessary packages
library(janitor)
library(lubridate)
library(tidyverse)
library(shiny)
library(palmerpenguins)

# Import the required data for analysis
dig_df <- read.csv("DIG.csv") %>%
  clean_names()

# Define UI
ui <- fluidPage(
  titlePanel("DIG Trial: Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plotChoice", "Choose Scatterplot", choices = c(
        "Age vs Ejection Fraction",
        "Age vs BMI",
        "Age vs Serum Potassium Level",
        "Age vs Serum Creatinine",
        "Age vs Heart Rate",
        "Age vs Diastolic BP",
        "Age vs Systolic BP"
      ))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Summary",
                 fluidRow(
                   box(width = 12, title = "Data Summary", collapsible = TRUE, 
                       dataTableOutput("summaryTable"))
                 )
        ),
        tabPanel("Mortality Analysis",
                 fluidRow(
                   box(width = 12, title = "Mortality Analysis", collapsible = TRUE, 
                       plotOutput("mortalityPlot"))
                 )
        ),
        tabPanel("Hospitalization Analysis",
                 fluidRow(
                   box(width = 12, title = "Hospitalization Analysis", collapsible = TRUE, 
                       plotOutput("hospitalizationPlot"))
                 )
        ),
        tabPanel("Scatterplot Analysis",
                 fluidRow(
                   box(width = 12, title = "Scatterplot Analysis", collapsible = TRUE, 
                       plotOutput("selectedPlot"))
                 )
        ),
        tabPanel("Hired Summary",
                 fluidRow(
                   box(width = 12, title = "Hired Summary", collapsible = TRUE, 
                       dataTableOutput("hiredSummary"))
                 )
        )
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  output$summaryTable <- renderDataTable({
    head(dig_df)
  })
  
  output$mortalityPlot <- renderPlot({
    if (!"TRTMT" %in% names(dig_df) || !"DEATH" %in% names(dig_df)) {
      return(NULL)
    }
    
    ggplot(dig_df, aes(x = factor(TRTMT), fill = factor(DEATH))) +
      geom_bar(position = "fill") +
      labs(x = "Treatment Group", y = "Proportion", fill = "Death") +
      scale_fill_brewer(palette = "Set1")
  })
  
  output$hospitalizationPlot <- renderPlot({
    if (!"TRTMT" %in% names(dig_df) || !"HOSP" %in% names(dig_df)) {
      return(NULL)
    }
    
    ggplot(dig_df, aes(x = factor(TRTMT), fill = factor(HOSP))) +
      geom_bar(position = "fill") +
      labs(x = "Treatment Group", y = "Proportion", fill = "Hospitalization") +
      scale_fill_brewer(palette = "Set1")
  })

  output$selectedPlot <- renderPlot({
    if (!input$plotChoice %in% c("Age vs Ejection Fraction", "Age vs BMI", 
                                 "Age vs Serum Potassium Level", "Age vs Serum Creatinine", 
                                 "Age vs Heart Rate", "Age vs Diastolic BP", "Age vs Systolic BP")) {
      return(NULL)
    }
    
    plot_data <- switch(input$plotChoice,
                        "Age vs Ejection Fraction" = {
                          if (!"AGE" %in% names(dig_df) || !"EJF_PER" %in% names(dig_df)) {
                            return(NULL)
                          }
                          ggplot(dig_df, aes(x = AGE, y = EJF_PER)) +
                            geom_point() +
                            labs(x = "Age", y = "Ejection Fraction", title = "Age vs Ejection Fraction")
                        },
                        "Age vs BMI" = {
                          if (!"AGE" %in% names(dig_df) || !"BMI" %in% names(dig_df)) {
                            return(NULL)
                          }
                          ggplot(dig_df, aes(x = AGE, y = BMI)) +
                            geom_point() +
                            labs(x = "Age", y = "BMI", title = "Age vs BMI")
                        },
                        "Age vs Serum Potassium Level" = {
                          if (!"AGE" %in% names(dig_df) || !"KLEVEL" %in% names(dig_df)) {
                            return(NULL)
                          }
                          ggplot(dig_df, aes(x = AGE, y = KLEVEL)) +
                            geom_point() +
                            labs(x = "Age", y = "Serum Potassium Level", title = "Age vs Serum Potassium Level")
                        },
                        "Age vs Serum Creatinine" = {
                          if (!"AGE" %in% names(dig_df) || !"CREAT" %in% names(dig_df)) {
                            return(NULL)
                          }
                          ggplot(dig_df, aes(x = AGE, y = CREAT)) +
                            geom_point() +
                            labs(x = "Age", y = "Serum Creatinine", title = "Age vs Serum Creatinine")
                        },
                        "Age vs Heart Rate" = {
                          if (!"AGE" %in% names(dig_df) || !"HEARTRTE" %in% names(dig_df)) {
                            return(NULL)
                          }
                          ggplot(dig_df, aes(x = AGE, y = HEARTRTE)) +
                            geom_point() +
                            labs(x = "Age", y = "Heart Rate", title = "Age vs Heart Rate")
                        },
                        "Age vs Diastolic BP" = {
                          if (!"AGE" %in% names(dig_df) || !"DIABP" %in% names(dig_df)) {
                            return(NULL)
                          }
                          ggplot(dig_df, aes(x = AGE, y = DIABP)) +
                            geom_point() +
                            labs(x = "Age", y = "Diastolic BP", title = "Age vs Diastolic BP")
                        },
                        "Age vs Systolic BP" = {
                          if (!"AGE" %in% names(dig_df) || !"SYSBP" %in% names(dig_df)) {
                            return(NULL)
                          }
                          ggplot(dig_df, aes(x = AGE, y = SYSBP)) +
                            geom_point() +
                            labs(x = "Age", y = "Systolic BP", title = "Age vs Systolic BP")
                        }
    )
    plot_data
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
