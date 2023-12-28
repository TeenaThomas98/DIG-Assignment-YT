dig_data <- dig_df

server <- function(input, output) {
  dig_filtered <- reactive({
    data <- dig_data
    if (input$sex != "All") {
      data <- data %>% filter(SEX == input$sex)
    }
    data %>% filter(AGE >= input$age[1] & AGE <= input$age[2])
  })
  
  output$plot1 <- renderPlot({ 
    ggplot(data = dig_filtered(), aes(x = BMI, y = AGE)) +
      geom_point() +
      labs(title = "Age vs BMI", x = "BMI", y = "Age")
  })
  
  output$table1 <- renderDataTable({ 
    dig_filtered()
  }, options = list(pageLength = 10)) # 添加分页选项
}