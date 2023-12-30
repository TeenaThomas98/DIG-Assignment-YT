
# DIG Data Analysis Shiny App

## Overview

This repository contains a Shiny application designed for the analysis of the DIG dataset. The app offers an interactive platform for exploring various aspects of the data, including treatment summaries, baseline characteristics, variable distributions, and more. It's aimed at researchers, data analysts, and anyone interested in the DIG trial data.

## Features

- **Treatment Summary**: Visualize the distribution of patients across different treatment groups.
- **Baseline Characteristics**: Explore patient demographics and baseline characteristics.
- **Variable Distributions**: Analyze the distribution of various variables in the dataset.
- **Statistical Summaries**: View statistical summaries of selected variables.
- **Boxplot and Scatterplot Analyses**: Examine relationships between different variables through various plots.
- **DIG Trial Analysis**: Detailed analysis of the DIG trial, including mortality and hospitalization summaries.

## Installation

To run this Shiny app locally, you need to have R installed on your computer. Follow these steps:

1. Clone this repository to your local machine.
2. Open R or RStudio and set the working directory to the folder where you cloned the repository.
3. Install the required R packages using the following commands:

   ```R
   install.packages(c("shiny", "shinydashboard", "tidyverse", "DT", "viridis", "janitor", "lubridate", "palmerpenguins", "DynNom"))
   ```

4. Run the app using the command:

   ```R
   shiny::runApp()
   ```

## Usage

Once the app is running:

1. Use the sidebar to filter data based on sex, treatment group, age range, and variables to display.
2. Explore different tabs to view treatment summaries, variable distributions, statistical summaries, and various analyses.
3. Use interactive elements like sliders, checkboxes, and dropdown menus to adjust the data and plots displayed.

## Contributing

This project is a collaborative effort by Yunjie Luo and Teena Thomas. Contributions to this project are welcome and appreciated. If you are interested in contributing, please ensure that you align with the standards and practices established by Yunjie Luo and Teena Thomas. 


## Contact

For any queries or suggestions, please contact .
Yunjie Luo: 1249354609@qq.com
Teena Thomas: thomas.teena1998@gmail.com

## Acknowledgments

This project was built using data from the DIG trial and utilizes various R packages like Shiny, Tidyverse, and others. Thanks to all the contributors of these packages.
