library(readxl)
library(fdth)

####################################
########## Data loading ############
####################################

data <- read_excel("B:/BSMRAAU 22024010 5th/MAT_4509/Programming/United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xls", range="b2:w158")
data <- as.data.frame(data)

wages_rows <- c(8, 47, 86, 125)-2

####################################
########## Functions ###############
####################################

get_row <- function(row, data) {
  return(na.omit(as.numeric(data[row,-1]))[1:10])
}

create_table <- function(data){
    interval <- as.integer((max(data)-min(data))/(log2(length(data))+1))
    table <- fdt(pilots_wages,start=min(data), end=max(data), h=interval)
    plot(table)
    return(table)
}

####################################
########## Main code ###############
####################################

tables <- list()

for (i in wages_rows){
    pilots_wages <- get_row(i,data)
    tables <- append(tables, create_table(pilots_wages))
}
print(tables[3])