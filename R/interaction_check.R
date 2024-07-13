# Load necessary libraries
library(dplyr)
library(purrr)
library(broom) # For tidy output of statistical tests

# Function to calculate OR from a contingency table
calculate_or <- function(table) {
  # Convert the table to a matrix
  table_matrix <- as.matrix(table)
  
  # Calculate the OR
  or <- (table_matrix[1,1] * table_matrix[2,2]) / (table_matrix[1,2] * table_matrix[2,1])
  
  # Return the OR
  return(or)
}

# Function to perform the analysis for a given level of the variable
analyze_by_level <- function(df, level) {
  df_filtered <- df %>% filter(sex == level)
  
  # Create the contingency table
  contingency_table <- with(df_filtered, table(depression, bzd))
  
  # Calculate the OR
  or <- calculate_or(contingency_table)
  
  return(tibble(sex = level, OR = or))
}

# Get the unique levels of the variable (sex in this case)
levels <- unique(df_regression$sex)

# Perform the analysis for each level
result <- map_dfr(levels, ~ analyze_by_level(df_regression, .x))

# View the result
print(result)
