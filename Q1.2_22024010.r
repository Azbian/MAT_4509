library(readxl)

# Load data
data <- read_excel("B:/BSMRAAU 22024010 5th/MAT_4509/Programming/United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xls", range = "b2:w158")
data <- as.data.frame(data)

# Define rows to analyze
wages_rows <- c(8, 47, 86, 125) - 2
fleets <- c("Small Narrowbodies", "Large Narrowbodies", "Widebodies", "Total Fleet")

# Function to get the first 10 non-NA values from a specified row
get_row <- function(row, data) {
  na.omit(as.numeric(data[row, -1]))[1:10]
}

# Function to calculate mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Function to format numbers to 3 decimal places
format_number <- function(x) {
  format(round(x, 3), nsmall = 3)
}

# Calculate statistics for specified rows
results_list <- lapply(wages_rows, function(i) {
  wages <- get_row(i, data)
  list(
    mean = format_number(mean(wages, na.rm = TRUE)),
    mode = format_number(get_mode(wages)),
    median = format_number(median(wages, na.rm = TRUE)),
    range = format_number(max(wages, na.rm = TRUE) - min(wages, na.rm = TRUE)),
    sd = format_number(sd(wages, na.rm = TRUE)),
    variance = format_number(var(wages, na.rm = TRUE)),
    quartiles = paste(format_number(quantile(wages, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)), collapse = ", "),
    deciles = paste(format_number(quantile(wages, probs = c(0.9, 1), na.rm = TRUE)), collapse = ", ")
  )
})

# Convert results into a matrix format
results_matrix <- do.call(cbind, lapply(results_list, function(x) {
  c(x$mean, x$mode, x$median, x$range, x$sd, x$variance, x$quartiles, x$deciles)
}))

# Assign row names and column names
rownames(results_matrix) <- c("mean", "mode", "median", "range", "sd", "variance", "quartiles", "deciles")
colnames(results_matrix) <- fleets

# Convert the matrix to data frame for better printing
results_df <- as.data.frame(results_matrix)

# Print the results in the desired format
print(results_df, row.names = TRUE)
