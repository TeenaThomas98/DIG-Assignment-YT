#install packages
if (!require("janitor")) install.packages("janitor")
if (!require("lubridate")) install.packages("lubridate")
if (!require("shiny")) install.packages("shiny")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("palmerpenguins")) install.packages("palmerpenguins")
if (!require("DynNom")) install.packages("DynNom")
if (!require("shinydashboard")) install.packages("shinydashboard")

#Load necessary packages
library(janitor)
library(lubridate)
library(tidyverse)
library(shiny)
library(palmerpenguins)
library(dplyr)
library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
library(reshape2)
library(shinythemes)

#Import the required data for analysis
dig_df <- read.csv("DIG.csv") %>%
  janitor::clean_names()
dig_df

# Select the specified columns
selected_columns <- dig_df %>%
  select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY)

# Convert columns to the most relevant data type
selected_columns$ID <- as.integer(selected_columns$ID)
selected_columns$TRTMT <- as.integer(selected_columns$TRTMT)
selected_columns$AGE <- as.integer(selected_columns$AGE)
selected_columns$SEX <- as.integer(selected_columns$SEX)
selected_columns$BMI <- as.numeric(selected_columns$BMI)
selected_columns$KLEVEL <- as.numeric(selected_columns$KLEVEL)
selected_columns$CREAT <- as.numeric(selected_columns$CREAT)
selected_columns$DIABP <- as.integer(selected_columns$DIABP)
selected_columns$SYSBP <- as.integer(selected_columns$SYSBP)
selected_columns$HYPERTEN <- as.integer(selected_columns$HYPERTEN)
selected_columns$CVD <- as.integer(selected_columns$CVD)
selected_columns$WHF <- as.integer(selected_columns$WHF)
selected_columns$DIG <- as.integer(selected_columns$DIG)
selected_columns$HOSP <- as.integer(selected_columns$HOSP)
selected_columns$HOSPDAYS <- as.integer(selected_columns$HOSPDAYS)
selected_columns$DEATH <- as.integer(selected_columns$DEATH)
selected_columns$DEATHDAY <- as.integer(selected_columns$DEATHDAY)

# View the data types
print(selected_columns)



dig_df$TRTMT <- factor(dig_df$TRTMT, levels = c(0, 1), labels = c("Placebo", "Treatment"))
dig_df$RACE <- factor(dig_df$RACE, levels = c(1, 2), labels = c("White", "Nonwhite"))
dig_df$SEX <- factor(dig_df$SEX, levels = c(1, 2), labels = c("Male", "Female"))
dig_df$HYPERTEN <- factor(dig_df$HYPERTEN, levels = c(0, 1), labels = c("No", "Yes"))
dig_df$CVD <- factor(dig_df$CVD, levels = c(0, 1), labels = c("No", "Yes"))
dig_df$DEATH <- factor(dig_df$DEATH, levels = c(0, 1), labels = c("Alive", "Death"))
dig_df$WHF <- factor(dig_df$WHF, levels = c(0, 1), labels = c("No", "Yes"))
dig_df$DIG <- factor(dig_df$DIG, levels = c(0, 1), labels = c("No", "Yes"))
dig_df$HOSP <- factor(dig_df$HOSP, levels = c(0, 1), labels = c("No", "Yes"))

print(dig_df)


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
        "Age vs Systolic BP",
        "Age vs NYHA Functional Class"
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
    ggplot(dig_df, aes(x = factor(TRTMT), fill = factor(DEATH))) +
      geom_bar(position = "fill") +
      labs(x = "Treatment Group", y = "Proportion", fill = "Death") +
      scale_fill_brewer(palette = "Set1")
  })
  
  output$hospitalizationPlot <- renderPlot({
    ggplot(dig_df, aes(x = factor(TRTMT), fill = factor(HOSP))) +
      geom_bar(position = "fill") +
      labs(x = "Treatment Group", y = "Proportion", fill = "Hospitalization") +
      scale_fill_brewer(palette = "Set1")
  })

  output$selectedPlot <- renderPlot({
  plot_data <- switch(input$plotChoice,
                      "Age vs Ejection Fraction" = {
                        ggplot(dig_df, aes(x = AGE, y = EJF_PER)) +
                          geom_point() +
                          labs(x = "Age", y = "Ejection Fraction", title = "Age vs Ejection Fraction")
                      },
                      "Age vs BMI" = {
                        ggplot(dig_df, aes(x = AGE, y = BMI)) +
                          geom_point() +
                          labs(x = "Age", y = "BMI", title = "Age vs BMI")
                      },
                      "Age vs Serum Potassium Level" = {
                        ggplot(dig_df, aes(x = AGE, y = KLEVEL)) +
                          geom_point() +
                          labs(x = "Age", y = "Serum Potassium Level", title = "Age vs Serum Potassium Level")
                      },
                      "Age vs Serum Creatinine" = {
                        ggplot(dig_df, aes(x = AGE, y = CREAT)) +
                          geom_point() +
                          labs(x = "Age", y = "Serum Creatinine", title = "Age vs Serum Creatinine")
                      },
                      "Age vs Heart Rate" = {
                        ggplot(dig_df, aes(x = AGE, y = HEARTRTE)) +
                          geom_point() +
                          labs(x = "Age", y = "Heart Rate", title = "Age vs Heart Rate")
                      },
                      "Age vs Diastolic BP" = {
                        ggplot(dig_df, aes(x = AGE, y = DIABP)) +
                          geom_point() +
                          labs(x = "Age", y = "Diastolic BP", title = "Age vs Diastolic BP")
                      },
                      "Age vs Systolic BP" = {
                        ggplot(dig_df, aes(x = AGE, y = SYSBP)) +
                          geom_point() +
                          labs(x = "Age", y = "Systolic BP", title = "Age vs Systolic BP")
                      },
                      "Age vs NYHA Functional Class" = {
                        ggplot(dig_df, aes(x = AGE, y = FUNCTCLS)) +
                          geom_point() +
                          labs(x = "Age", y = "NYHA Functional Class", title = "Age vs NYHA Functional Class")
                      }
  )
  plot_data
})

# Run the Shiny app
shinyApp(ui = ui, server = server)

