
if (!require("janitor")) install.packages("janitor")
if (!require("lubridate")) install.packages("lubridate")
if (!require("shiny")) install.packages("shiny")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("palmerpenguins")) install.packages("palmerpenguins")
if (!require("DynNom")) install.packages("DynNom")
if (!require("shinydashboard")) install.packages("shinydashboard")


library(janitor)
library(lubridate)
library(tidyverse)
library(shiny)
library(palmerpenguins)
library(dplyr)
library(readr)
library(dplyr)
library(forcats)

#Import the required data for analysis
dig_df <- read.csv("DIG.csv") %>%
  janitor::clean_names()
dig_df

# Read the CSV file
dig_df <- read_csv("DIG.csv")

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






dig_data <- dig_df

ui <- fluidPage(
  titlePanel("Digitalis Investigation Group Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      # Add input controls relevant to your data
      selectInput(inputId = "sex", label = "Select Sex:", choices = unique(dig_data$SEX), multiple = FALSE),
      sliderInput("age", "Select Age Range:", min = min(dig_data$AGE, na.rm = TRUE), max = max(dig_data$AGE, na.rm = TRUE), value = c(50, 60)),
      # ... add more filters if needed ...
    ),
    mainPanel(
      plotOutput("plot1"),
      dataTableOutput("table1")
    )
  )
)

server <- function(input, output) {
  
  dig_filtered <- reactive({
    dig_data %>%
      filter(SEX == input$sex) %>%
      filter(AGE >= input$age[1] & AGE <= input$age[2])
    # ... add more filters if needed ...
  })
  
  output$plot1 <- renderPlot({ 
    ggplot(data = dig_filtered(), aes(x = BMI, y = AGE)) +
      geom_point()
  })
  
  output$table1 <- renderDataTable({ 
    dig_filtered()
  })
}

shinyApp(ui, server)
