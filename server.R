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


# Server Definition
server <- function(input, output, session) ({
  dig_df <- read.csv("DIG.csv")
  filtered_data <- reactive({
    invalidateLater(10000, session)
    req(input$refresh)
    data <- dig_df
    if (input$sex != "All") {
      data <- data[data$SEX == ifelse(input$sex == "Male(1)", 1, 2), ]
    }
    if (input$treatment_group != "All") {
      data <- data[data$TRTMT == as.numeric(ifelse(input$treatment_group == "Treatment(1)", 1, 0)), ]
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
  
  # Refresh action
  observeEvent(input$refresh, {
    # This can be used to trigger reactive expressions
  })
  
  
  # Additional plot logic
  output$selectedAdditionalPlot <- renderPlot({
    req(input$additionalPlotChoice)
    selected_data <- dig_df
    
    switch(input$additionalPlotChoice,
           "Ejection Fraction across NYHA Functional Class" = {
             selected_data$FUNCTCLS <- label_nyha(selected_data$FUNCTCLS)
             ggplot(selected_data, aes(x = FUNCTCLS, y = EJF_PER)) +
               geom_boxplot() +
               labs(x = "NYHA Functional Class", y = "Ejection Fraction", title = "Ejection Fraction across NYHA Functional Class")
           },
           "BMI across Sex" = {
             selected_data$SEX <- factor(selected_data$SEX, labels = c("Male", "Female"))
             ggplot(selected_data, aes(x = SEX, y = BMI)) +
               geom_boxplot() +
               labs(x = "Sex", y = "BMI", title = "BMI across Sex")
           },
           "Ejection Fraction across Race" = {
             selected_data$RACE <- factor(selected_data$RACE, labels = c("White", "Non-White"))
             ggplot(selected_data, aes(x = RACE, y = EJF_PER)) +
               geom_boxplot() +
               labs(x = "Race", y = "Ejection Fraction", title = "Ejection Fraction across Race")
           },  
           "Serum Creatinine across Previous MI" = {
             selected_data$PREVMI <- label_prevmi(selected_data$PREVMI)
             ggplot(selected_data, aes(x = PREVMI, y = CREAT)) +
               geom_boxplot() +
               labs(x = "Previous MI", y = "Serum Creatinine", title = "Serum Creatinine across Previous MI")
           },
           "Serum Creatinine across Diabetes" = {
             selected_data$SEX <- label_diabetes(selected_data$DIABETES)
             ggplot(selected_data, aes(x = DIABETES, y = CREAT)) +
               geom_boxplot() +
               labs(x = "Diabetes", y = "Serum Creatinine", title = "Serum Creatinine across Diabetes")
           }
    )                     
  })
  
  
  # selectedScatterPlot
  output$selectedScatterPlot <- renderPlot({
    req(input$scatterPlotChoice)
    data <- dig_df
    
    switch(input$scatterPlotChoice,
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
    )#switch
  })
  
  
  
}) 