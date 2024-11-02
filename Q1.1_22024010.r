library(readxl)
library(fdth)

# Load data
data <- read_excel("B:/BSMRAAU 22024010 5th/MAT_4509/Programming/United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xls", range="b2:w158")
data <- as.data.frame(data)

fleets <- c("Small Narrowbodies", "Large Narrowbodies", "Widebodies", "Total Fleet")
wages_rows <- c(8, 47, 86, 125) - 2

# Function to get the first 10 non-NA values from a specified row
get_row <- function(row, data) {
  na.omit(as.numeric(data[row, -1]))[1:10]
}

# Function to create and save frequency distribution tables
create_table <- function(data, fleet_name) {
  interval <- as.integer((max(data) - min(data)) / (log2(length(data)) + 1))
  table <- fdt(data, start = min(data), end = max(data), h = interval)
  plot(table, main = paste("Wages Distribution for", fleet_name))
}

# Create the directory if it doesn't exist
output_dir <- "B:/BSMRAAU 22024010 5th/MAT_4509/Programming/Results"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)  # Create the directory
}

# Save tables in a PDF
pdf(file.path(output_dir, "Wages_Distribution.pdf"), width = 8, height = 6)

# Create and plot tables for each fleet
for (i in seq_along(wages_rows)) {
  pilots_wages <- get_row(wages_rows[i], data)
  create_table(pilots_wages, fleets[i])
}

dev.off()  # Close the PDF device
