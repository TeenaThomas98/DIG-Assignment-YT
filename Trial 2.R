# Load necessary libraries
library(shiny)
library(ggplot2)
library(reshape2)  # Required for data manipulation if not already loaded

# Read the DIG dataset (replace 'path_to_data.csv' with your file path)
data <- read.csv("DIG.csv")

# Define UI
ui <- fluidPage(
  titlePanel("DIG Trial: Scatterplot Analysis"),
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
      plotOutput("selectedPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$selectedPlot <- renderPlot({
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
}

# Run the application
shinyApp(ui = ui, server = server)
