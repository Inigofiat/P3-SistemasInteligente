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
library(gridExtra)
library(kableExtra)

# Prints a table with minimum, maximum, and mean values for each variable
# in a dataset.
print.data.summary <- function(data) {
  # Obtain a summary of the data
  data_summary <- summary(data)
  data_summary

  # Extract the values of interest and transform them to numeric
  min_values <- round(as.numeric(gsub(".*:", "", data_summary[1, ])), 4)
  mean_values <- round(as.numeric(gsub(".*:", "", data_summary[4, ])), 4)
  max_values <- round(as.numeric(gsub(".*:", "", data_summary[6, ])), 4)

  # Create a data.frame with the results
  summary_df <- data.frame(
    Variable = names(data),
    Min = min_values,
    Max = max_values,
    Mean = mean_values
  )

  # Create a table with the results
  print(kable(summary_df, caption = "Summary of Data",
            col.names = c("Variable", "MIN.", "MAX.", "MEAN")) %>%
            kable_styling(full_width = TRUE, position = "left"))
}

# Prints a table with the correlation between the target variable and
# the rest of the columns of a dataset.
print.data.correlations <- function(data, target_variable) {
  # Calculate the correlation between the target variable and the rest of the columns
  correlations <- sapply(data, function(col) round(cor(data[[target_variable]], col), 4))

  # Create a data frame with the results
  correlation_table <- data.frame(Variable = names(data),
                                  Correlation = correlations,
                                  row.names = NULL)

  # Add a column for color formatting
  correlation_table$Color <- ifelse(correlation_table$Correlation >= -0.2 &
                                    correlation_table$Correlation <= 0.2, "red", "green")

  # Create a table with the results
  print(kable(correlation_table[, c("Variable", "Correlation")],
          caption = "Correlation between variables ('green' means good correlation, >= 0.2)") %>%
          kable_styling(full_width = TRUE, position = "left") %>%
          column_spec(1, color = correlation_table$Color) %>%
          column_spec(2, color = correlation_table$Color))
}

# Plot the distribution of the variables in the dataset using scatter.smooth
plot.data.distribution <- function(data, target_variable) {
  # Calculate the number of rows and columns for the grid
  num_variables <- ncol(data) - 1
  num_rows <- ceiling(sqrt(num_variables))
  num_cols <- ceiling(num_variables / num_rows)

  par(mfrow = c(num_rows, num_cols), mar=c(1,1,1,1))

  # Create a scatter plot for each variable
  for (col_name in colnames(data)) {
    if (col_name != target_variable) {
      scatter.smooth(x = data[[target_variable]],
                     y = data[[col_name]],
                     main = col_name,
                     col = "lightgreen")
    }
  }
}

print.model.sumary <- function(model) {
  # Obtain a summary of the model
  model_summary <- summary(model)


  # Print Mean Absolute Error
  print(paste0("- Adjusted R-squared: ", model_summary$adj.r.squared))

  # Create a data frame witheach varaible, its coefficient, and p-value
  coefficients_table <- data.frame(
    Variable = rownames(model_summary$coefficients),
    Coefficient = round(model_summary$coefficients[, "Estimate"], 4),
    p_value = round(model_summary$coefficients[, "Pr(>|t|)"], 4),
    row.names = NULL
  )

  # Add a column for color formatting
  coefficients_table$Color <- ifelse(coefficients_table$p_value >= 0.05, "red", "green")

  # Create a table with the results
  print(kable(coefficients_table[, c("Variable", "Coefficient", "p_value")],
        caption = "Coefficients of the model ('green' means meaningful varaible, p_value &lt; 0.05)") %>%
        kable_styling(full_width = TRUE, position = "left") %>%
        column_spec(1, color = coefficients_table$Color) %>%
        column_spec(2, color = coefficients_table$Color) %>%
        column_spec(3, color = coefficients_table$Color))
}
