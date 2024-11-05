library(readxl)

####################################
########## Data Loading ############
####################################

data <- read_excel("B:/BSMRAAU 22024010 5th/MAT_4509/Programming/United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xls", range="b2:w158") 
data <- as.data.frame(data)

fleets <- c("Small Narrowbodies", "Large Narrowbodies", "Widebodies", "Total Fleet")
purchased_goods_row <- matrix(c(14,15,16,53,54,55,92,93,94,131,132,133)-2,nrow=4,byrow=TRUE)
purchased_goods_names<- c("Fuel/Oil", "Insurance", "Others (Inc. Tax)")
aircraft_ownership_rows <- matrix(c(26,27,65,66,104,105,143,144)-2,nrow=4,byrow=TRUE)
aircraft_ownership_names <- c("Rentals", "Depreciation and Amortization")
daily_utilization_rows <- matrix(c(39,40,41,78,79,80,117,118,119,156,157,158)-2,nrow=4,byrow=TRUE)
daily_utilization_names <- c("Block hours", "Airborne hours", "Depurtures")

####################################
########## Functions ###############
####################################

get_row <- function(row, data) {
  return(na.omit(as.numeric(data[row, -1]))[1:10])
}

get_box_plot <- function(rows,data,group_names,fleets){
    dims <- dim(rows)
    for(i in 1:dims[1]){
        values <- list()
        for(j in 1:dims[2]){
            row_data <- list(get_row(rows[i,j],data))
            values <- append(values,row_data)
        }
        boxplot(values,names=group_names, main=fleets[i],xlab = "Groups", ylab = "Values")
    }
}

get_box_plot(purchased_goods_row,data,purchased_goods_names,fleets)
get_box_plot(aircraft_ownership_rows,data,aircraft_ownership_names,fleets)
get_box_plot(daily_utilization_rows,data,daily_utilization_names,fleets)