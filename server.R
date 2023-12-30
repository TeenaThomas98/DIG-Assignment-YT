# Server Definition
data <- read.csv("DIG.csv")
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    invalidateLater(2000, session)
    req(input$refresh)
    data <- dig_df
    if (input$sex != "All") {
      data <- data[data$SEX == ifelse(input$sex == "Male", 1, 2), ]
    }
    if (input$treatment_group != "All") {
      data <- data[data$TRTMT == as.numeric(ifelse(input$treatment_group == "Treatment", 1, 0)), ]
    }
    data <- data[data$AGE >= input$age_range[1] & data$AGE <= input$age_range[2], ]
    return(data)
  })
  
  # Plot for treatment summary with colorful bars
  output$treatmentPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = as.factor(TRTMT), fill = as.factor(TRTMT))) +
      geom_bar() +
      scale_fill_viridis(discrete = TRUE, option = "D") +
      labs(title = "Treatment Summary", x = "Treatment Group", y = "Count") +
      theme_minimal()
  })
  
  # Table for baseline characteristics
  output$baselineTable <- renderDT({
    datatable(filtered_data() %>%
                select(all_of(input$variables)))
  })
  
  # Plot for variable distributions with colorful histograms
  output$distributionPlot <- renderPlot({
    if (length(input$variables) > 0) {
      ggplot(filtered_data(), aes_string(x = input$variables[1], fill = input$variables[1])) +
        geom_histogram(binwidth = 1) +
        scale_fill_viridis(discrete = TRUE) +
        labs(title = paste("Distribution of", input$variables[1])) +
        theme_minimal()
    }
  })
  
  # Statistical summary
  output$statSummary <- renderPrint({
    req(input$variables)
    summary(filtered_data()[, input$variables])
  })
  
  
  
  
  
  #selectedPlot1
  
  output$selectedPlot1 <- renderPlot({
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
  
  
  
  #selectedPlot2
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
  
  
  output$selectedPlot2 <- renderPlot({
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
                        },
                        "Ejection Fraction across NYHA Functional Class" = {
                          data$FUNCTCLS <- label_nyha(data$FUNCTCLS)
                          ggplot(data, aes(x = FUNCTCLS, y = EJF_PER)) +
                            geom_boxplot() +
                            labs(x = "NYHA Functional Class", y = "Ejection Fraction", title = "Ejection Fraction across NYHA Functional Class")
                        },
                        "BMI across Sex" = {
                          data$SEX <- factor(data$SEX, labels = c("Male", "Female"))
                          ggplot(data, aes(x = SEX, y = BMI)) +
                            geom_boxplot() +
                            labs(x = "Sex", y = "BMI", title = "BMI across Sex")
                        },
                        "Ejection Fraction across Race" = {
                          data$RACE <- factor(data$RACE, labels = c("White", "Non-White"))
                          ggplot(data, aes(x = RACE, y = EJF_PER)) +
                            geom_boxplot() +
                            labs(x = "Race", y = "Ejection Fraction", title = "Ejection Fraction across Race")
                        },
                        "Serum Creatinine across Previous MI" = {
                          data$PREVMI <- label_prevmi(data$PREVMI)
                          ggplot(data, aes(x = PREVMI, y = CREAT)) +
                            geom_boxplot() +
                            labs(x = "Previous MI", y = "Serum Creatinine", title = "Serum Creatinine across Previous MI")
                        },
                        "Serum Creatinine across Diabetes" = {
                          data$DIABETES <- label_diabetes(data$DIABETES)
                          ggplot(data, aes(x = DIABETES, y = CREAT)) +
                            geom_boxplot() +
                            labs(x = "Diabetes", y = "Serum Creatinine", title = "Serum Creatinine across Diabetes")
                        },
                        "Heart Rate across Medication Usage" = {
                          data$MEDICATION_USAGE <- label_medication(data$MEDICATIONS)
                          ggplot(data, aes(x = MEDICATION_USAGE, y = HEARTRTE)) +
                            geom_boxplot() +
                            labs(x = "Medication Usage", y = "Heart Rate", title = "Heart Rate across Medication Usage")
                        }
    )
    plot_data
  })
  
  
  
  # Refresh action
  observeEvent(input$refresh, {
    # can be used to trigger reactive expressions
  }) 
  
  
}