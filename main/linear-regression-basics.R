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

# Load ggplot2 library
library(ggplot2)

# Clear console
cat("\014")
# Clear plots
if(!is.null(dev.list())) dev.off()
graphics.off()
# Clean workspace
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Define the training set (for 2 degrees)
training_set    <- data.frame(x1 = seq(1:20))
training_set$x0 <- 1 # this is auxiliar
training_set$target  <- 2 * training_set$x1 - 3
# Another training set with noise (uncomment to run)
training_set$target <- training_set$target + 4 * runif(nrow(training_set), 0, 1) - 2

# Define weights for the learning hypotesis and alpha
w0 <- 0
w1 <- 0
alpha <- 0.01

# Calculate the hypothesis using current weights
training_set$hypothesis <- w0 + w1 * training_set$x1
# Calculate the current error
training_set$error <- training_set$target - training_set$hypothesis

# Show training set in the command line
training_set

# Plot data and hypothesis before applaying Linear Regression
ggplot(training_set) +
  geom_point(aes(x = x1, y = target, col = "Target")) +
  geom_point(aes(x = x1, y = hypothesis, col = "Hypothesis")) +
  labs(title = paste0("Initial situation"),
       caption = paste0("Coefficients: w1=", round(w1, 3),"  w0=", round(w0, 3))) +
  scale_color_manual(values = c("Target" = "green", "Hypothesis" = "orange"))

# Update weights
w1 <- w1 + alpha * sum(training_set$error * training_set$x1) / nrow(training_set)
w0 <- w0 + alpha * sum(training_set$error * training_set$x0) / nrow(training_set)

# Update the hypothesis value and the error
training_set$hypothesis <- w0 + w1 * training_set$x1
training_set$error <- training_set$target - training_set$hypothesis

# Show training set in the command line
training_set

# Plot data and hypothesis after updating the weights one time
ggplot(training_set) +
  geom_point(aes(x = x1, y = target, col = "Target")) +
  geom_point(aes(x = x1, y = hypothesis, col = "Hypothesis")) +
  labs(title = paste0("After 1 update"),
       caption = paste0("Coefficients: w1=", round(w1, 3),"  w0=", round(w0, 3))) +
  scale_color_manual(values = c("Target" = "green", "Hypothesis" = "orange"))

# Set the number of iterations for the learning process
iterations <- 2000

for (i in 1:iterations) {
  # Update weights
  w1 <- w1 + alpha * sum(training_set$error * training_set$x1) / nrow(training_set)
  w0 <- w0 + alpha * sum(training_set$error * training_set$x0) / nrow(training_set)
  # Update the hypothesis value and the error
  training_set$hypothesis <- w0 + w1 * training_set$x1
  training_set$error <- training_set$target - training_set$hypothesis
  # Print the evolution of the weigths and error during the learnig process
  print(paste0(i, "  w1=", round(w1, 3),
               "  w0=", round(w0, 3),
               "  error=", round(mean(abs(training_set$error)), 3)), quote = FALSE)
}

# Plot data and hypothesis after finishing the learning process
ggplot(training_set) +
  geom_point(aes(x = x1, y = target, col = "Target")) +
  geom_point(aes(x = x1, y = hypothesis, col = "Hypothesis")) +
  labs(title = paste0("After ", i, " updates"),
       caption = paste0("Coefficients: w1=", round(w1, 3),"  w0=", round(w0, 3))) +
  scale_color_manual(values = c("Target" = "green", "Hypothesis" = "orange"))

