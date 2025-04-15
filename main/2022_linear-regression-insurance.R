# -----------------------------------------------------------------------------
# This code snippet was generated for educational purposes as part of the
# Intelligent Systems course at the University of Deusto. The code has been
# created with assistance from ChatGPT version 3.5 and GitHub Copilot.
#
# The code is released under the Creative Commons License and is provided
# for free use and modification by the programming and development community.
#
# This script was generated in April 2024, the year when the Athletic Club de
# Bilbao won the 25th King's Cup.
# -----------------------------------------------------------------------------

# Install required packages
library(lattice)
library(ggplot2)
library(caret)

# Clear console
cat("\014")
# Clear plots
if(!is.null(dev.list())) dev.off()
graphics.off()
# Clean workspace
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load custom functions
source("linear-regression-utils.R")

#-----------------------
# READ AND PREPARE DATA
#-----------------------
data <- read.csv("../data/2022_insurance.csv")

# Sex is omitted because it is not a numerical value.
data$sex <- NULL
# The region is omitted because it is not a numerical value.
data$region <- NULL

# Change smoker "yes" by 1 and "no" by 0
data$smoker[data$smoker == "yes"] <- 1
data$smoker[data$smoker == "no"] <- 0
data[,'smoker'] <- as.numeric(data[,'smoker'])

#----------------------
# PRELIMINARY ANALYSIS
#----------------------
# Print data summary
print.data.summary(data)
# Print data correlations
print.data.correlations(data, "charges")
# Plot data distribution
plot.data.distribution(data, "charges")

#-----------------------------------
# GENERATA AND ANALYZE LINEAR MODEL
#-----------------------------------
# Initialize variables
total_avg_error <- 0
best_model      <- NULL
min_avg_error   <- max(data$charges)
training_p      <- 0.80

# Repeat the process 10 times
for (i in 1:10) {
  # Generate data partition 80% training / 20% test. The result is a vector with
  # the indexes of the examples that will be used for the training of the model.
  training_samples <- createDataPartition(y = data$charges, p = training_p, list = FALSE)
  # Split training and test data
  training_data    <- data[training_samples, ]
  test_data        <- data[-training_samples, ]
  # Create Linear Model using training data. Formula = all the columns except charges
  model <- lm(formula = data$charges ~., data = data)
  # Make the prediction using the model and test data
  prediction       <- predict(model, test_data)
  # Calculate Mean Average Error
  mean_avg_error   <- mean(abs(prediction - test_data$charges))
  # Print Mean Absolute Error
  print(paste0("- Mean average error of model'", i, ": ", mean_avg_error))
  # Update the best model and the minimum average error
  if (mean_avg_error < min_avg_error) {
    min_avg_error <- mean_avg_error
    best_model <- model
  }
  # Update the total average error
  total_avg_error <- total_avg_error + mean_avg_error
}

# Calculate the total average error
total_avg_error <- (total_avg_error / 10)
# Print total and best average error
print(paste0("- Total average error: ", total_avg_error))
print(paste0("- Best average error: ", min_avg_error))

# Print standard summary of the best model
summary(model)
# Print summary of the best model
print.model.sumary(model)

# Make the prediction using the model and test data
prediction       <- predict(model, test_data)
# Calculate de prediction error
prediction_error <- abs(prediction - test_data$charges)
# Obtain the index of the example with the highest error
index_max_error  <- which.max(prediction_error)
# Print the example with the highest error
print(test_data[index_max_error, ])
# Print the error of the example with the highest error
print(paste0("Real charges: ", round(test_data$charges[index_max_error], 4),
             " / Prediction: ", round(prediction[index_max_error], 4),
             " / inc.: ", round(prediction_error[index_max_error], 4)))

#------------------------------
# ANSWER THEORETICAL QUESTIONS
#------------------------------

# If a person stops smoking, how much would the charges be reduced?
data_new_NO <- data[data$smoker == 1, ]
data_new_NO$smoker <- 0
prediction_new_NO   <- predict(best_model, data_new_NO)
mean_error_new_NO   <- mean(data_new_NO$charges - prediction_new_NO)
print(paste0("- Mean average reduction: ", mean_error_new_NO))

# How much would the charges increase if one person starts smoking?
data_new_YES <- data[data$smoker == 0, ]
data_new_YES$smoker <- 1
prediction_new_YES  <- predict(best_model, data_new_YES)
mean_error_new_YES  <- mean(prediction_new_YES - data_new_YES$charges)
print(paste0("- Mean average increase: ", mean_error_new_YES))

# Who are the 3 people whose charges will increase the most in 5 years?
data_new_5_years       <- data
data_new_5_years$age   <- data_new_5_years$age + 5
prediction_new_5_years <- predict(best_model, data_new_5_years)
inc_charges_5_years    <- prediction_new_5_years - data_new_5_years$charges
index_max_inc          <- tail(order(inc_charges_5_years), 3)

print(data_new_5_years[index_max_inc, ])

for (index in index_max_inc) {
  print(paste0("Initial charges: ", round(data_new_5_years$charges[index], 4),
               " / new charges: ", round(prediction_new_5_years[index], 4),
               " / inc.: ", round(inc_charges_5_years[index], 4)))
}
