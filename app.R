#necessary packages
if (!require("janitor")) install.packages("janitor")
if (!require("lubridate")) install.packages("lubridate")
if (!require("shiny")) install.packages("shiny")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("palmerpenguins")) install.packages("palmerpenguins")
if (!require("DynNom")) install.packages("DynNom")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("DT")) install.packages("DT")

library(janitor)
library(lubridate)
library(tidyverse)
library(shiny)
library(palmerpenguins)
library(dplyr)
library(readr)
library(forcats)
library(ggplot2)
library(reshape2)
library(shinythemes)
library(DT)
library(shinydashboard)

#Import the required data for analysis
dig_df <- read.csv("DIG.csv") %>%
  janitor::clean_names()
dig_df

# Convert columns to the most relevant data type
dig_df <- dig_df %>%
  mutate(
    ID = as.integer(ID),
    TRTMT = as.integer(TRTMT),
    AGE = as.integer(AGE),
    SEX = as.integer(SEX),
    BMI = as.numeric(BMI),
    KLEVEL = as.numeric(KLEVEL),
    CREAT = as.numeric(CREAT),
    DIABP = as.integer(DIABP),
    SYSBP = as.integer(SYSBP),
    HYPERTEN = as.integer(HYPERTEN),
    CVD = as.integer(CVD),
    WHF = as.integer(WHF),
    DIG = as.integer(DIG),
    HOSP = as.integer(HOSP),
    HOSPDAYS = as.integer(HOSPDAYS),
    DEATH = as.integer(DEATH),
    DEATHDAY = as.integer(DEATHDAY)
  )



dig_df$TRTMT <- factor(dig_df$TRTMT, levels = c(0, 1), labels = c("Placebo", "Treatment"))
dig_df$RACE <- factor(dig_df$RACE, levels = c(1, 2), labels = c("White", "Nonwhite"))
dig_df$SEX <- factor(dig_df$SEX, levels = c(1, 2), labels = c("Male", "Female"))
dig_df$HYPERTEN <- factor(dig_df$HYPERTEN, levels = c(0, 1), labels = c("No", "Yes"))
dig_df$CVD <- factor(dig_df$CVD, levels = c(0, 1), labels = c("No", "Yes"))
dig_df$DEATH <- factor(dig_df$DEATH, levels = c(0, 1), labels = c("Alive", "Death"))
dig_df$WHF <- factor(dig_df$WHF, levels = c(0, 1), labels = c("No", "Yes"))
dig_df$DIG <- factor(dig_df$DIG, levels = c(0, 1), labels = c("No", "Yes"))
dig_df$HOSP <- factor(dig_df$HOSP, levels = c(0, 1), labels = c("No", "Yes"))



#shiny
dig_data <- dig_df
shinyApp(ui, server)
