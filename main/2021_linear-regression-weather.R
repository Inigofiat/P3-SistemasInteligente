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
# Read data from CSV
filename = "../data/2021_loiu_2000-01_2021-03.csv"
data <- read.csv(file=filename, sep=",", header = TRUE)

# Remove non-numerical columns of the data
data$fecha <- NULL
data$indicativo <- NULL
data$nombre <- NULL
data$provincia <- NULL
data$altitud <- NULL
data$horatmin <- NULL
data$horatmax <- NULL
data$horaracha <- NULL
data$sol <- NULL
data$horaPresMax <- NULL
data$horaPresMin <- NULL

#----------------------
# PRELIMINARY ANALYSIS
#----------------------
# Print data summary
print.data.summary(data)
# Print data correlations
print.data.correlations(data, "tmed")
# Plot data distribution
plot.data.distribution(data, "tmed")

#-----------------------------------
# GENERATA AND ANALYZE LINEAR MODEL
#-----------------------------------
# Percentaje of training examples
training_p <- 0.80

# Generate data partition 80% training / 20% test. The result is a vector with
# the indexes of the examples that will be used for the training of the model.
training_indexes <- createDataPartition(y = data$tmed, p = training_p, list = FALSE)

# Split training and test data
training_data <- data[training_indexes, ]
test_data     <- data[-training_indexes, ]

# Create Linear Model using training data. Formula = all the columns except tmed
model <- lm(formula = tmed~., data = training_data)

# Make the prediction using the model and test data
prediction <- predict(model, test_data)

# Calculate Mean Average Error
mean_avg_error <- mean(abs(prediction - test_data$tmed))

# Print Mean Absolute Error
print(paste0("- Mean average error: ", mean_avg_error))

# Print standard summary of the model
summary(model)

# Print summary of the model
print.model.sumary(model)
