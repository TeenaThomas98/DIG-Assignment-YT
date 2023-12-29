dig_data <- dig_df


server <- function(input, output) {
  # summary
  output$treatment_summary <- renderPrint({
    dig_data %>% 
      group_by(TRTMT) %>% 
      summarise(Count = n(), Proportion = n() / nrow(dig_data))
  })
  
  # Mortality Analysis
  output$mortality_plot <- renderPlot({
    ggplot(dig_data, aes(x = factor(TRTMT), fill = factor(DEATH))) +
      geom_bar(position = "fill") +
      labs(x = "Treatment Group", y = "Proportion", fill = "Death") +
      scale_fill_brewer(palette = "Set1")
  })
  
  # Hospitalization Analysis
  output$hospitalization_plot <- renderPlot({
    ggplot(dig_data, aes(x = factor(TRTMT), fill = factor(HOSP))) +
      geom_bar(position = "fill") +
      labs(x = "Treatment Group", y = "Proportion", fill = "Hospitalization") +
      scale_fill_brewer(palette = "Set1")
  })
  
  # add more
}