library(readxl)

data <- read_excel("B:/BSMRAAU 22024010 5th/MAT_4509/Programming/United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xls", range = "b2:w158")
data <- as.data.frame(data)

get_row <- function(row, data) {
  na.omit(as.numeric(data[row, -1]))[1:10]
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

years_row <- 3 - 2
load_factor_rows <- c(36, 75, 114, 153) - 2
fleets <- c("Small Narrowbodies", "Large Narrowbodies", "Widebodies", "Total Fleet")
save_path <- "B:/BSMRAAU 22024010 5th/MAT_4509/Programming/Charts"

if (!dir.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

create_barplot(years_row, load_factor_rows, fleets, save_path)
