library(readxl)
library(fdth)

# Load data
data <- read_excel("B:/BSMRAAU 22024010 5th/MAT_4509/Programming/United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xls", range="b2:w158")
data <- as.data.frame(data)
wages_rows <- c(8, 47, 86, 125) - 2

# Function to get the first 10 non-NA values from a specified row
get_row <- function(row, data) {
  na.omit(as.numeric(data[row, -1]))[1:10]
}

# Function to calculate mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate statistics for specified rows
results <- sapply(wages_rows, function(i) {
  wages <- get_row(i, data)
  c(mean = mean(wages),
    mode = get_mode(wages),
    median = median(wages),
    range=max(wages)-min(wages),
    sd = sd(wages),
    variance = var(wages),
    quartiles = list(quantile(wages, probs = c(0.25, 0.5, 0.75))),
    deciles = list(quantile(wages, probs = c(0.9, 1))))
})

# Print all results
print(results)
