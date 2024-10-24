# Constants for atomic masses (in g/mol)
CARBON_MASS <- 12.0107
HYDROGEN_MASS <- 1.00794
CHLORINE_MASS <- 35.453

calculate_ratio <- function(chlorine_count, carbon_count) {
  hydrogen_count <- 2 * carbon_count + 2 - chlorine_count
  total_chlorine_mass <- CHLORINE_MASS * chlorine_count
  total_carbon_mass <- CARBON_MASS * carbon_count
  total_hydrogen_mass <- HYDROGEN_MASS * hydrogen_count
  numerator <- total_chlorine_mass
  denominator <- total_carbon_mass + total_hydrogen_mass + total_chlorine_mass
  return(numerator / denominator)
}

# Range of values for chlorine and carbon
chlorine_range <- 3:30
carbon_range <- 6:36

# Initialize the matrix to store the results
results <- matrix(nrow = length(carbon_range), ncol = length(chlorine_range) + 1)

# Add headers
colnames(results) <- c("Carbon Count / Chlorine Count", paste0("Cl", chlorine_range))
rownames(results) <- paste0("C", carbon_range)

# Fill the matrix
for (i in seq_along(carbon_range)) {
  carbon_count <- carbon_range[i]
  row <- numeric(length(chlorine_range) + 1)
  row[1] <- paste0("C", carbon_count)
  for (j in seq_along(chlorine_range)) {
    chlorine_count <- chlorine_range[j]
    ratio <- calculate_ratio(chlorine_count, carbon_count)
    row[j + 1] <- ratio
  }
  results[i, ] <- row
}

# Convert the matrix to a data frame for CSV writing
results_df <- as.data.frame(results, stringsAsFactors = FALSE)

# Write the CSV file
write.csv(results_df, "data/chlorine_carbon_matrix.csv", row.names = FALSE)

cat("The CSV file 'chlorine_carbon_matrix.csv' has been successfully generated.\n")