#necessary packages
if (!require("janitor")) install.packages("janitor")
if (!require("lubridate")) install.packages("lubridate")
if (!require("shiny")) install.packages("shiny")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("palmerpenguins")) install.packages("palmerpenguins")
if (!require("DynNom")) install.packages("DynNom")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("DT")) install.packages("DT")
if (!require("viridis")) install.packages("viridis")


library(janitor)
library(lubridate)
library(tidyverse)
library(shiny)
library(palmerpenguins)
library(dplyr)
library(readr)
library(forcats)
library(ggplot2)
library(reshape2)
library(shinythemes)
library(DT)
library(shinydashboard)
library(viridis)

dig_df <- read.csv("DIG.csv")

# Convert columns to the most relevant data type
dig_df <- dig_df %>%
  mutate(
    ID = as.integer(ID),
    TRTMT = as.integer(TRTMT),
    AGE = as.integer(AGE),
    SEX = as.integer(SEX),
    BMI = as.numeric(BMI),
    KLEVEL = as.numeric(KLEVEL),
    CREAT = as.numeric(CREAT),
    DIABP = as.integer(DIABP),
    SYSBP = as.integer(SYSBP),
    HYPERTEN = as.integer(HYPERTEN),
    CVD = as.integer(CVD),
    WHF = as.integer(WHF),
    DIG = as.integer(DIG),
    HOSP = as.integer(HOSP),
    HOSPDAYS = as.integer(HOSPDAYS),
    DEATH = as.integer(DEATH),
    DEATHDAY = as.integer(DEATHDAY)
  )
dig_df$TRTMT <- factor(dig_df$TRTMT, levels = c(0, 1), labels = c("Placebo", "Treatment"))
dig_df$RACE <- factor(dig_df$RACE, levels = c(1, 2), labels = c("White", "Nonwhite"))
dig_df$SEX <- factor(dig_df$SEX, levels = c(1, 2), labels = c("Male", "Female"))
dig_df$HYPERTEN <- factor(dig_df$HYPERTEN, levels = c(0, 1), labels = c("No", "Yes"))
dig_df$CVD <- factor(dig_df$CVD, levels = c(0, 1), labels = c("No", "Yes"))
dig_df$DEATH <- factor(dig_df$DEATH, levels = c(0, 1), labels = c("Alive", "Death"))
dig_df$WHF <- factor(dig_df$WHF, levels = c(0, 1), labels = c("No", "Yes"))
dig_df$DIG <- factor(dig_df$DIG, levels = c(0, 1), labels = c("No", "Yes"))
dig_df$HOSP <- factor(dig_df$HOSP, levels = c(0, 1), labels = c("No", "Yes"))


dig_data <- dig_df





# Function to label categorical variables
label_prevmi <- function(x) {
  ifelse(x == 1, "Previous MI", "No Previous MI")
}

label_diabetes <- function(x) {
  ifelse(x == 1, "Diabetes", "No Diabetes")
}

label_medication <- function(x) {
  ifelse(x == 0, "No Medication", "Medication Used")
}

label_nyha <- function(x) {
  # Assuming NYHA Functional Class is already in the dataset as FUNCTCLS
  factor(x, levels = c(1, 2, 3, 4), labels = c("Class I", "Class II", "Class III", "Class IV"))
}



# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "DIG Data Analysis"),
  dashboardSidebar(
    radioButtons(inputId = "sex", label = "Select Patient Sex:", choices = c("All", "Male", "Female")),
    selectInput(inputId = "treatment_group", label = "Select Treatment Group:", choices = c("All", "Placebo", "Treatment"), multiple = FALSE),
    sliderInput("age_range", "Select Age Range:", min = 0, max = 100, value = c(20, 40)),
    checkboxGroupInput(inputId = "variables", label = "Select Variables to Display:", choices = c("AGE", "SEX", "BMI", "TRTMT"), selected = c("AGE", "SEX", "BMI", "TRTMT")),
    actionButton("refresh", "Refresh Data")
  ),
  dashboardBody(
    tabBox(
      width = 12,
      id = "tabs",
      tabPanel("Summary", 
               fluidRow(
                 box(width=12, 
                     title = "Treatment Summary", 
                     collapsible = TRUE, 
                     status = "primary", 
                     solidHeader = TRUE,
                     plotOutput("treatmentPlot")),
                 box(width=12,
                     title = "Baseline Characteristics", 
                     collapsible = TRUE, 
                     status = "primary", 
                     solidHeader = TRUE,
                     DTOutput("baselineTable"))
               )
      ),
      tabPanel("Plots", 
               fluidRow(
                 box(width=12, 
                     title = "Variable Distributions", 
                     collapsible = TRUE, 
                     status = "primary", 
                     solidHeader = TRUE,
                     plotOutput("distributionPlot")),
                 box(width=12, 
                     title = "Statistical Summary", 
                     collapsible = TRUE, 
                     status = "primary", 
                     solidHeader = TRUE,
                     verbatimTextOutput("statSummary"))
               )
      ),
      tabPanel("Scatterplot Analysis", 
               fluidRow(
                 box(
                   width = 12,
                   id = "tabs",
                   tabPanel("Scatterplot Analysis", 
                            fluidRow(
                              box(width = 12, status = "primary", solidHeader = TRUE, 
                                  selectInput("plotChoice", "Choose Scatterplot", choices = c(
                                    "Age vs Ejection Fraction" = "EJF_PER",
                                    "Age vs BMI" = "BMI",
                                    "Age vs Serum Potassium Level" = "KLEVEL",
                                    "Age vs Serum Creatinine" = "CREAT",
                                    "Age vs Heart Rate" = "HEARTRTE",
                                    "Age vs Diastolic BP" = "DIABP",
                                    "Age vs Systolic BP" = "SYSBP",
                                    "Age vs NYHA Functional Class" = "NYHA"
                                  )),
                                  plotOutput("selectedPlot1")
                              )
                            )
                   )
                 )
               )
      ),
      
      
      tabPanel("Boxplot/Violin Plot Analysis", 
               fluidRow(
                 box(
                   width = 12,
                   id = "tabs",
                   tabPanel("Boxplot/Violin Plot Analysis", 
                            fluidRow(
                              box(width = 12, status = "primary", solidHeader = TRUE, 
                                  selectInput("plotChoice", "Choose Plot", choices = c(
                                    "Ejection Fraction across NYHA Functional Class",
                                    "BMI across Sex",
                                    "Ejection Fraction across Race",
                                    "Serum Creatinine across Previous MI",
                                    "Serum Creatinine across Diabetes"
                                  )),
                                  plotOutput("selectedPlot2")
                              )
                            )
                   )
                 )
               ),
      )     
    )
  ))

# Server Definition
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    invalidateLater(2000, session)
    req(input$refresh)
    data <- dig_df
    if (input$sex != "All") {
      data <- data[data$SEX == ifelse(input$sex == "Male", 1, 2), ]
    }
    if (input$treatment_group != "All") {
      data <- data[data$TRTMT == as.numeric(ifelse(input$treatment_group == "Treatment", 1, 0)), ]
    }
    data <- data[data$AGE >= input$age_range[1] & data$AGE <= input$age_range[2], ]
    return(data)
  })
  
  # Plot for treatment summary with colorful bars
  output$treatmentPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = as.factor(TRTMT), fill = as.factor(TRTMT))) +
      geom_bar() +
      scale_fill_viridis(discrete = TRUE, option = "D") +
      labs(title = "Treatment Summary", x = "Treatment Group", y = "Count") +
      theme_minimal()
  })
  
  # Table for baseline characteristics
  output$baselineTable <- renderDT({
    datatable(filtered_data() %>%
                select(all_of(input$variables)))
  })
  
  # Plot for variable distributions with colorful histograms
  output$distributionPlot <- renderPlot({
    if (length(input$variables) > 0) {
      ggplot(filtered_data(), aes_string(x = input$variables[1], fill = input$variables[1])) +
        geom_histogram(binwidth = 1) +
        scale_fill_viridis(discrete = TRUE) +
        labs(title = paste("Distribution of", input$variables[1])) +
        theme_minimal()
    }
  })
  
  # Statistical summary
  output$statSummary <- renderPrint({
    req(input$variables)
    summary(filtered_data()[, input$variables])
  })
  
  
  
  # Refresh action
  observeEvent(input$refresh, {
    # can be used to trigger reactive expressions
  })
  
  #selectedPlot1
  output$selectedPlot1 <- renderPlot({
    plot_data <- switch(input$plotChoice,
                        "Age vs Ejection Fraction" = {
                          ggplot(data, aes(x = AGE, y = EJF_PER)) +
                            geom_point() +
                            labs(x = "Age", y = "Ejection Fraction", title = "Age vs Ejection Fraction")
                        },
                        "Age vs BMI" = {
                          ggplot(data, aes(x = AGE, y = BMI)) +
                            geom_point() +
                            labs(x = "Age", y = "BMI", title = "Age vs BMI")
                        },
                        "Age vs Serum Potassium Level" = {
                          ggplot(data, aes(x = AGE, y = KLEVEL)) +
                            geom_point() +
                            labs(x = "Age", y = "Serum Potassium Level", title = "Age vs Serum Potassium Level")
                        },
                        "Age vs Serum Creatinine" = {
                          ggplot(data, aes(x = AGE, y = CREAT)) +
                            geom_point() +
                            labs(x = "Age", y = "Serum Creatinine", title = "Age vs Serum Creatinine")
                        },
                        "Age vs Heart Rate" = {
                          ggplot(data, aes(x = AGE, y = HEARTRTE)) +
                            geom_point() +
                            labs(x = "Age", y = "Heart Rate", title = "Age vs Heart Rate")
                        },
                        "Age vs Diastolic BP" = {
                          ggplot(data, aes(x = AGE, y = DIABP)) +
                            geom_point() +
                            labs(x = "Age", y = "Diastolic BP", title = "Age vs Diastolic BP")
                        },
                        "Age vs Systolic BP" = {
                          ggplot(data, aes(x = AGE, y = SYSBP)) +
                            geom_point() +
                            labs(x = "Age", y = "Systolic BP", title = "Age vs Systolic BP")
                        },
                        "Age vs NYHA Functional Class" = {
                          ggplot(data, aes(x = AGE, y = FUNCTCLS)) +
                            geom_point() +
                            labs(x = "Age", y = "NYHA Functional Class", title = "Age vs NYHA Functional Class")
                        }
    )
    print(plot_data)
  })
  
  
  
  #selectedPlot2
  output$selectedPlot2 <- renderPlot({
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
                        },
                        "Ejection Fraction across NYHA Functional Class" = {
                          data$FUNCTCLS <- label_nyha(data$FUNCTCLS)
                          ggplot(data, aes(x = FUNCTCLS, y = EJF_PER)) +
                            geom_boxplot() +
                            labs(x = "NYHA Functional Class", y = "Ejection Fraction", title = "Ejection Fraction across NYHA Functional Class")
                        },
                        "BMI across Sex" = {
                          data$SEX <- factor(data$SEX, labels = c("Male", "Female"))
                          ggplot(data, aes(x = SEX, y = BMI)) +
                            geom_boxplot() +
                            labs(x = "Sex", y = "BMI", title = "BMI across Sex")
                        },
                        "Ejection Fraction across Race" = {
                          data$RACE <- factor(data$RACE, labels = c("White", "Non-White"))
                          ggplot(data, aes(x = RACE, y = EJF_PER)) +
                            geom_boxplot() +
                            labs(x = "Race", y = "Ejection Fraction", title = "Ejection Fraction across Race")
                        },
                        "Serum Creatinine across Previous MI" = {
                          data$PREVMI <- label_prevmi(data$PREVMI)
                          ggplot(data, aes(x = PREVMI, y = CREAT)) +
                            geom_boxplot() +
                            labs(x = "Previous MI", y = "Serum Creatinine", title = "Serum Creatinine across Previous MI")
                        },
                        "Serum Creatinine across Diabetes" = {
                          data$DIABETES <- label_diabetes(data$DIABETES)
                          ggplot(data, aes(x = DIABETES, y = CREAT)) +
                            geom_boxplot() +
                            labs(x = "Diabetes", y = "Serum Creatinine", title = "Serum Creatinine across Diabetes")
                        },
                        "Heart Rate across Medication Usage" = {
                          data$MEDICATION_USAGE <- label_medication(data$MEDICATIONS)
                          ggplot(data, aes(x = MEDICATION_USAGE, y = HEARTRTE)) +
                            geom_boxplot() +
                            labs(x = "Medication Usage", y = "Heart Rate", title = "Heart Rate across Medication Usage")
                        }
    )
    plot_data
  })
  
  
}

# Run the app
shinyApp(ui = ui, server = server)