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
  factor(x, levels = c(1, 2, 3, 4), labels = c("Class I", "Class II", "Class III", "Class IV"))
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "DIG Data Analysis"),
  dashboardSidebar(
    radioButtons(inputId = "sex", label = "Select Patient Sex:", choices = c("All", "Male(1)", "Female(2)")),
    selectInput(inputId = "treatment_group", label = "Select Treatment Group:", choices = c("All", "Placebo(0)", "Treatment(1)"), multiple = FALSE),
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
      tabPanel("Boxplot Plot Analysis",
               fluidRow(
                 box(width = 12, status = "primary", solidHeader = TRUE,
                     selectInput("additionalPlotChoice", "Choose Boxplot Plot", choices = c(
                       "Ejection Fraction across NYHA Functional Class",
                       "BMI across Sex",
                       "Ejection Fraction across Race",
                       "Serum Creatinine across Previous MI",
                       "Serum Creatinine across Diabetes"
                     )),
                     plotOutput("selectedAdditionalPlot")
                 )
               )
      ),
      
      
      
      tabPanel("Scatterplot Analysis",
               fluidRow(
                 box(width = 12, status = "primary", solidHeader = TRUE,
                     selectInput("scatterPlotChoice", "Choose Scatterplot", choices = c(
                       "Age vs Ejection Fraction",
                       "Age vs BMI",
                       "Age vs Serum Potassium Level",
                       "Age vs Serum Creatinine",
                       "Age vs Heart Rate",
                       "Age vs Diastolic BP",
                       "Age vs Systolic BP",
                       "Age vs NYHA Functional Class"                     
                     )),
                     plotOutput("selectedScatterPlot")
                 )
               )
               
      )
    )
  )
)S