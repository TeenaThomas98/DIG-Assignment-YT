# UI Definition
data <- read.csv("DIG.csv")
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
                   title = "Scatterplot Analysis",
                   collapsible = TRUE,
                   status = "primary", 
                   solidHeader = TRUE, 
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
                           "Age vs NYHA Functional Class" = "NYHA")
                         )
                     )
                   ),
                   plotOutput("selectedPlot1")
                 )
               )
               
      ),
      tabPanel("Boxplot/Violin Plot Analysis", 
               fluidRow(
                 box(
                   width = 12,
                   title = "Boxplot/Violin Plot Analysis",
                   collapsible = TRUE,
                   status = "primary", 
                   solidHeader = TRUE,
                   fluidRow(
                     box(width = 12, status = "primary", solidHeader = TRUE, 
                         selectInput("plotChoice", "Choose Plot", choices = c(
                           "Ejection Fraction across NYHA Functional Class",
                           "BMI across Sex",
                           "Ejection Fraction across Race",
                           "Serum Creatinine across Previous MI",
                           "Serum Creatinine across Diabetes")
                         )
                     )
                   ),
                   plotOutput("selectedPlot2")
                 )
               )
      )#tabPanel Plot Analysis
      
      
    )#tabBox
  )#dashboardBody
)#ui