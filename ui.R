dig_data <- dig_df


ui <- dashboardPage(
    dashboardHeader(title = "DIG Data Explorer"),
    dashboardSidebar(
      # 根据 DIG 数据添加适当的筛选器
      selectInput(inputId = "treatment", label = "Select Treatment Group:", choices = unique(dig_data$TRTMT)),
      sliderInput("age", "Select Age Range:", min = min(dig_data$AGE, na.rm = TRUE), max = max(dig_data$AGE, na.rm = TRUE), value = c(50, 60))
      # 添加其他筛选器
    ),
    dashboardBody(
    tabBox(width = 12, id = "tabs",
      tabPanel("Data Summary", 
               fluidRow(box(width=12, title = "Data Summary", collapsible = TRUE, status = "warning", solidHeader = TRUE,
            dataTableOutput("summaryTable")))
      ),
      tabPanel("Mortality Analysis", 
               fluidRow(
                box(width=12, title = "Mortality Analysis", collapsible = TRUE, status = "warning", solidHeader = TRUE,
            plotOutput("mortalityPlot")) 
               )
      ),
      tabPanel("Hospitalization Analysis", 
               fluidRow(
                box(width=12, title = "Hospitalization Analysis", collapsible = TRUE, status = "warning", solidHeader = TRUE,
            plotOutput("hospitalizationPlot"))
               )
      ),
      # add more
      
    )
  )
)

