library(readxl)

####################################
########## Data Loading ############
####################################

data <- read_excel("B:/BSMRAAU 22024010 5th/MAT_4509/Programming/United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xls", range="b2:w158") 
data <- as.data.frame(data)

# Define fleet names and corresponding row indices
fleets <- c("Small Narrowbodies", "Large Narrowbodies", "Widebodies", "Total Fleet")
purchased_goods_row <- matrix(c(14, 15, 16, 53, 54, 55, 92, 93, 94, 131, 132, 133) - 2, nrow = 4, byrow = TRUE)
purchased_goods_names <- c("Fuel/Oil", "Insurance", "Others (Inc. Tax)")
aircraft_ownership_rows <- matrix(c(26, 27, 65, 66, 104, 105, 143, 144) - 2, nrow = 4, byrow = TRUE)
aircraft_ownership_names <- c("Rentals", "Depreciation and Amortization")
daily_utilization_rows <- matrix(c(39, 40, 41, 78, 79, 80, 117, 118, 119, 156, 157, 158) - 2, nrow = 4, byrow = TRUE)
daily_utilization_names <- c("Block hours", "Airborne hours", "Departures")

####################################
########## Functions ###############
####################################

get_row <- function(row, data) {
  na.omit(as.numeric(data[row, -1]))[1:10]
}

save_path <- "B:/BSMRAAU 22024010 5th/MAT_4509/Programming/Question_01/Charts"

# Function to create and save box plots to a PDF
create_box_plot_pdf <- function(rows, data, group_names, fleets, file_name) {
  pdf(file.path(save_path, file_name), width = 8, height = 6)
  colors <- c("lightblue", "lightgreen", "lightpink")
  for (i in 1:nrow(rows)) {
    values <- lapply(rows[i, ], get_row, data = data)  # Get data for all groups in a row
    boxplot(values, names = group_names, main = fleets[i], xlab = "Groups", ylab = "Values", col=colors[1:ncol(rows)])
  }
  
  dev.off()  # Close the PDF device
}

# Generate the box plots and save them as PDFs
create_box_plot_pdf(purchased_goods_row, data, purchased_goods_names, fleets, "Purchased_Goods_Box_Plot.pdf")
create_box_plot_pdf(aircraft_ownership_rows, data, aircraft_ownership_names, fleets, "Aircraft_Ownership_Box_Plot.pdf")
create_box_plot_pdf(daily_utilization_rows, data, daily_utilization_names, fleets, "Daily_Utilization_Box_Plot.pdf")
