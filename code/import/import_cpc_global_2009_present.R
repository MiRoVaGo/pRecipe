library(ncdf4)
library(raster)
library(data.table)
library(dplyr)
library(SDMTools)


year_select <- "2010"
start_date <- paste0(year_select, "-01-01")
end_date <- paste0(year_select, "-12-31")
days_array <- seq(as.Date(start_date), as.Date(end_date), by="days") %>% format("%Y-%m-%d")
days_index <- which(days_array %in% c(start_date, end_date))
days_index <- days_index[1]:days_index[2]
data_folder <- paste0("../data/CPC_global/", year_select)
all_files <- list.files(data_folder, pattern = "*.RT")
for (i in days_index){
  file_name <- paste0(data_folder, "/", all_files[i])
  con <- file(file_name, "rb")
  dummie <- readBin(con, numeric(), n = 360*720*2, size = 4)
  close(con)
  dummie <- dummie[1:(360*720)]
  dummie <- matrix(dummie, ncol = 360, nrow = 720)
  dummie[dummie < 0] <- NA
  dummie <- ifelse(dummie > 0, dummie * 0.1, dummie)
  attr(dummie, "xll") <- 0.25
  attr(dummie, "yll") <- -89.75
  attr(dummie, "cellsize") <- 0.5
  attr(dummie, "type") <- 'numeric'
  class(dummie) <- "asc"
  dummie <- raster.from.asc(dummie)
  if (i >= 2){
    precip <- stack(precip, dummie)
  } else {
    precip <- dummie      
  }
}