dig_data <- dig_df


# Server Definition
server <- function(input, output, session) {
  
  
  # Reactive expression to filter the dataset
  filtered_data <- reactive({
    invalidateLater(2000, session)
    req(input$refresh)
    data <- dig_df
    if (input$sex != "All") {
      data <- data[data$SEX == ifelse(input$sex == "Male", 1, 2), ]
    }
    if (input$treatment_group != "All") {
      data <- data[data$TRTMT == input$treatment_group, ]
    }
    data <- data[data$AGE >= input$age_range[1] & data$AGE <= input$age_range[2], ]
    return(data)
  })
  
  # Plot for treatment summary
  output$treatmentPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = TRTMT)) +
      geom_bar() +
      labs(title = "Treatment Summary", x = "Treatment Group", y = "Count")
  })
  
  # Table for baseline characteristics
  output$baselineTable <- renderDT({
    datatable(filtered_data() %>%
                select(all_of(input$variables)))
  })
  
  # Plot for variable distributions
  output$distributionPlot <- renderPlot({
    if (length(input$variables) > 0) {
      ggplot(filtered_data(), aes_string(x = input$variables[1])) +
        geom_histogram(binwidth = 1) +
        labs(title = paste("Distribution of", input$variables[1]))
    }
  })
  
  # Statistical summary
  output$statSummary <- renderPrint({
    req(input$variables)
    summary(filtered_data()[, input$variables])
  })
  
  # Refresh action
  observeEvent(input$refresh, {
    # This can be used to trigger reactive expressions
  })
}