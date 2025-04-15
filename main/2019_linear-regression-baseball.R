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

# Load required packages
library(lattice)
library(caret)
library(ggplot2)
library(gridExtra)
library(kableExtra)
library(RKEEL)

# Clear Environment
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014")

# Load custom functions
source("linear-regression-utils.R")

#-----------------------
# READ AND PREPARE DATA
#-----------------------
# Load data
data <- read.keel("../data/2019_baseball.dat")
# Convert string to integer
data <- transform(data, Salary = strtoi(Salary))
# Remove non-numerical columns of the data
data$Free_agency_eligibility <- NULL
data$Free_agent <- NULL
data$Arbitration_eligibility <- NULL
data$Arbitration <- NULL

#----------------------
# PRELIMINARY ANALYSIS
#----------------------
# Print data summary
print.data.summary(data)
# Print data correlations
print.data.correlations(data, "Salary")
# Plot data distribution
plot.data.distribution(data, "Salary")

#-----------------------------------
# GENERATA AND ANALYZE LINEAR MODEL
#-----------------------------------
# Percentage of training examples
training_p <- 0.8

# Generate data partition 80% training / 20% test. The result is a vector with
# the indexes of the examples that will be used for the training of the model.
training_indexes <- createDataPartition(y = data$Salary, p = training_p, list = FALSE)

# Split training and test data
training_data <- data[training_indexes, ]  # Extract training data using training_indexes
test_data     <- data[-training_indexes, ] # Extract data with the indexes not included in training_indexes

# Create Linear Model using training data. Formula = all the columns except Salary
model <- lm(formula = Salary ~., data = training_data)

# Make the prediction using the model and test data
prediction <- predict(model, test_data)

# Calculate Mean Average Error
mean_avg_error <- mean(abs(prediction - test_data$Salary))

# Print Mean Absolute Error
print(paste0("- Mean average error: ", mean_avg_error))

# Print standard summary of the model
summary(model)

# Print summary of the model
print.model.sumary(model)
