library(readxl)
####################################
########## Data Loading ############
####################################
data <- read_excel("B:/BSMRAAU 22024010 5th/MAT_4509/Programming/United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xls", range="b2:w158") 
data <- as.data.frame(data)
fleets <- c("Small Narrowbodies", "Large Narrowbodies", "Widebodies", "Total Fleet")
years_row <- 3 - 2
load_factor_rows <- c(36, 75, 114, 153) - 2
maintanance_rows <- matrix(c(19,20,21,23,58,59,60,62,97,98,99,101,136,137,138,140) - 2, nrow = 4, byrow = FALSE)
####################################
########## Functions ###############
####################################

get_row <- function(row, data) {
  na.omit(as.numeric(data[row, -1]))[1:10]
}

get_row_sum <- function(row, data) {
  sum(na.omit(as.numeric(data[row, -1]))[1:10])
}

get_totals <- function(row_matrix, data) {
  sapply(1:4, function(i) sapply(1:4, function(j) get_row_sum(row_matrix[i, j], data)))
}

plot_and_save_pies <- function(labour, materials, third_party, burden, labels, save_path) {
  for (i in 1:4) {
    values <- c(labour[i], materials[i], third_party[i], burden[i])
    percentages <- round(values / sum(values) * 100)
    pie_labels <- paste(c("Labour", "Materials", "Third party", "Burden"), percentages, "%")
    file_name <- paste0(save_path, "/", labels[i], "_maintenance_pie.png")
    # Open a PNG device and save the plot
    png(filename = file_name, width = 600, height = 600)
    pie(values, labels = pie_labels, main = labels[i], col = rainbow(length(values)))
    dev.off()  # Close the device to save the file
  }
}

create_barplot <- function(years_row, load_factor_rows, fleets, save_path) {
  years <- get_row(years_row, data)
  for (i in seq_along(load_factor_rows)) {
    load_factors <- get_row(load_factor_rows[i], data)
    fleet_name <- fleets[i]
    file_name <- paste0(save_path, "/barplot_", fleet_name, ".png")
    png(file_name, width = 800, height = 600)
    barplot(load_factors, names.arg = years, col = "skyblue",
            main = paste("Load Factors for", fleet_name),
            xlab = "Years", ylab = "Load Factors")
    dev.off()
  }
  message("Bar plots saved successfully in ", save_path)
}


# Define save path
save_path <- "B:/BSMRAAU 22024010 5th/MAT_4509/Programming/Question_01/Charts"
# Create directory if it doesn't exist
if (!dir.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}
# Calculate totals for each category
totals <- get_totals(maintanance_rows, data)
total_labour <- totals[1, ]
total_materials <- totals[2, ]
total_third_party <- totals[3, ]
total_burden <- totals[4, ]
# Generate and save pie charts for each fleet
plot_and_save_pies(total_labour, total_materials, total_third_party, total_burden, fleets, save_path)


if (!dir.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

create_barplot(years_row, load_factor_rows, fleets, save_path)