
# Qualitative Community of Practice session 1 Worksheet
## Instructions: Use what you learned in this session to edit the code I have provided below so that it 
## follows reproducible practices that we discussed in class. Edit or delete my code as needed.
## Use this as an opportunity to get practice working with R projects, the here package, Quarto, 
## and 'sourcing' code. Work with your peers and compare what you did. 

# NOTE: This code is out of date. I actually need histograms for i) all months and ii) the month of September. 

# Make plots

rm(list = ls()) # reset everything

## load the data 
library(here)
data <- read.csv("./session_1/airquality.csv")

## clean data 
library(dplyr)
library(tidyr)

data_clean <- data %>% 
  select(-c(rownames)) %>% # Remove the 'rownames' column, as we do not need it
  drop_na(Ozone) # Remove rows that have missing data

## Now make the plots
hist_ozone <- hist(data_clean$Ozone, col = "red", 
                   breaks = 40, main = "Histogram of ozone", xlab = "Value", 
                   axes = TRUE)
hist_solar <- hist(data_clean$Solar.R, col = "red", breaks = 40, 
                   main = "Histogram of solar radiation", 
                   xlab = "Value", axes = TRUE)
hist_wind <- hist(data_clean$Wind, col = "red", breaks = 40, 
                  main = "Histogram of wind speed", xlab = "Value", axes = TRUE)
hist_temp <- hist(data_clean$Temp, 
                  col = "red", breaks = 40, main = "Histogram of ozone", xlab = "Value", axes = TRUE)

### look at results for month 6

hist_ozone <- hist(data_clean$Ozone[data_clean$Month == 6], col = "red", 
                   breaks = 40, main = "Histogram of ozone", xlab = "Value", 
                   axes = TRUE)
hist_solar <- hist(data_clean$Solar.R[data_clean$Month == 6], col = "red", breaks = 40, 
                   main = "Histogram of solar radiation", 
                   xlab = "Value", axes = TRUE)
hist_wind <- hist(data_clean$Wind[data_clean$Month == 6], col = "red", breaks = 40, 
                  main = "Histogram of wind speed", xlab = "Value", axes = TRUE)
hist_temp <- hist(data_clean$Temp[data_clean$Month == 6], 
                  col = "red", breaks = 40, main = "Histogram of ozone", xlab = "Value", axes = TRUE)



# Analyze the data

rm(list = ls()) # reset everything

## load data analysis libraries 
library(MASS)
library(lmtest)
library(mice)
library(mgcv)
library(lme4)
library(Hmisc)

## load the data
library(here)
data <- read.csv("./session_1/airquality.csv")

## clean data 
library(dplyr)
library(tidyr)
data_clean <- data %>% 
  select(-c(rownames)) %>% # Remove the 'rownames' column, as we do not need it
  drop_na(Ozone, Solar.R) # Remove rows that have missing data


### find relationship between ozone and temperature
model_simple <- lm(Ozone ~ Temp, data = data_clean)
results_matrix <- coef(summary(model_simple))
results_matrix

### Control for covariates 
model_covars <- lm(Ozone ~ Temp + Solar.R + Wind, data = data_clean)
results_matrix <- coef(summary(model_covars))
results_matrix


