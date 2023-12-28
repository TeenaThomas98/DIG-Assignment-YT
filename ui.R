dig_data <- dig_df

ui <- fluidPage(
  titlePanel("Digitalis Investigation Group Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "sex", label = "Select Sex:", choices = c("All", unique(dig_data$SEX)), selected = "All"),
      sliderInput("age", "Select Age Range:", min = min(dig_data$AGE, na.rm = TRUE), max = max(dig_data$AGE, na.rm = TRUE), value = c(50, 60))
      # 可以添加更多的过滤器
    ),
    mainPanel(
      plotOutput("plot1"),
      dataTableOutput("table1")
    )
  )
)
