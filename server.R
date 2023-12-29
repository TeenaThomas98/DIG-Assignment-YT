dig_data <- dig_df


# Define server
server <- function(input, output) {
  output$summaryTable <- renderDataTable({
    head(dig_df)
  })
  
  output$mortalityPlot <- renderPlot({
    ggplot(dig_df, aes(x = factor(TRTMT), fill = factor(DEATH))) +
      geom_bar(position = "fill") +
      labs(x = "Treatment Group", y = "Proportion", fill = "Death") +
      scale_fill_brewer(palette = "Set1")
  })
  
  output$hospitalizationPlot <- renderPlot({
    ggplot(dig_df, aes(x = factor(TRTMT), fill = factor(HOSP))) +
      geom_bar(position = "fill") +
      labs(x = "Treatment Group", y = "Proportion", fill = "Hospitalization") +
      scale_fill_brewer(palette = "Set1")
  })
  
  output$selectedPlot <- renderPlot({
    plot_data <- switch(input$plotChoice,
                        "Age vs Ejection Fraction" = {
                          ggplot(dig_df, aes(x = AGE, y = EJF_PER)) +
                            geom_point() +
                            labs(x = "Age", y = "Ejection Fraction", title = "Age vs Ejection Fraction")
                        },
                        "Age vs BMI" = {
                          ggplot(dig_df, aes(x = AGE, y = BMI)) +
                            geom_point() +
                            labs(x = "Age", y = "BMI", title = "Age vs BMI")
                        },
                        "Age vs Serum Potassium Level" = {
                          ggplot(dig_df, aes(x = AGE, y = KLEVEL)) +
                            geom_point() +
                            labs(x = "Age", y = "Serum Potassium Level", title = "Age vs Serum Potassium Level")
                        },
                        "Age vs Serum Creatinine" = {
                          ggplot(dig_df, aes(x = AGE, y = CREAT)) +
                            geom_point() +
                            labs(x = "Age", y = "Serum Creatinine", title = "Age vs Serum Creatinine")
                        },
                        "Age vs Heart Rate" = {
                          ggplot(dig_df, aes(x = AGE, y = HEARTRTE)) +
                            geom_point() +
                            labs(x = "Age", y = "Heart Rate", title = "Age vs Heart Rate")
                        },
                        "Age vs Diastolic BP" = {
                          ggplot(dig_df, aes(x = AGE, y = DIABP)) +
                            geom_point() +
                            labs(x = "Age", y = "Diastolic BP", title = "Age vs Diastolic BP")
                        },
                        "Age vs Systolic BP" = {
                          ggplot(dig_df, aes(x = AGE, y = SYSBP)) +
                            geom_point() +
                            labs(x = "Age", y = "Systolic BP", title = "Age vs Systolic BP")
                        },
                        "Age vs NYHA Functional Class" = {
                          ggplot(dig_df, aes(x = AGE, y = FUNCTCLS)) +
                            geom_point() +
                            labs(x = "Age", y = "NYHA Functional Class", title = "Age vs NYHA Functional Class")
                        }
    )
    plot_data
  })
}

  
  # add more