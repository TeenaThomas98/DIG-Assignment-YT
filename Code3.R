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
  output$selectedPlot <- renderPlot({
    plot_data <- switch(input$plotChoice,
                        "Ejection Fraction across NYHA Functional Class" = {
                          data$FUNCTCLS <- label_nyha(data$FUNCTCLS)
                          ggplot(data, aes(x = FUNCTCLS, y = EJF_PER)) +
                            geom_boxplot() +
                            labs(x = "NYHA Functional Class", y = "Ejection Fraction", title = "Ejection Fraction across NYHA Functional Class")
                        },
                        "BMI across Sex" = {
                          # Assuming Sex is already coded as SEX (1 = Male, 2 = Female)
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
                          # Assuming MEDICATIONS is already coded as MEDICATION_USAGE (0 = No Medication, 1 = Medication Used)
                          data$MEDICATION_USAGE <- label_medication(data$MEDICATIONS)
                          ggplot(data, aes(x = MEDICATION_USAGE, y = HEARTRTE)) +
                            geom_boxplot() +
                            labs(x = "Medication Usage", y = "Heart Rate", title = "Heart Rate across Medication Usage")
                        }
    )
    print(plot_data)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
