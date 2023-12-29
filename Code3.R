# Load necessary libraries
library(shiny)
library(ggplot2)
library(reshape2)  # Required for data manipulation if not already loaded

# Read the DIG dataset (replace 'path_to_data.csv' with your file path)
data <- read.csv("DIG.csv")

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

# Define UI
ui <- fluidPage(
  titlePanel("DIG Trial: Boxplot/Violin Plot Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plotChoice", "Choose Plot", choices = c(
        "Ejection Fraction across NYHA Functional Class",
        "BMI across Sex",
        "Ejection Fraction across Race",
        "Serum Creatinine across Previous MI",
        "Serum Creatinine across Diabetes",
        "Heart Rate across Medication Usage"
      ))
    ),
    mainPanel(
      plotOutput("selectedPlot")
    )
  )
)

# Define server logic
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


# Run the application
shinyApp(ui = ui, server = server)
