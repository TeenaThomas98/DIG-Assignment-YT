dig_data <- dig_df


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
        )
      )
    )
  )
)
