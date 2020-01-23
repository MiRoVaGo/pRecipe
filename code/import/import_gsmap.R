library(ncdf4)
library(raster)
library(data.table)
library(dplyr)
library(SDMTools)


year_select <- "2003"
month_select <- "01"
end_date <- as.Date(paste0(year_select, "-", month_select, "-01"))
days_index <- 1:days_in_month(end_date)
data_folder <- paste0("../data/GSMAP/", year_select,month_select)
all_files <- list.files(data_folder)
for (i in days_index){
  file_name <- paste0(data_folder, "/", all_files[i])
  con <- gzcon(file(file_name, "rb"))
  dummie <- readBin(con, numeric(), n = 3600*1200, size = 4)
  close(con)
  dummie <- matrix(dummie, ncol = 1200, nrow = 3600)
  dummie[dummie < 0] <- NA
  attr(dummie, "xll") <- 0.05
  attr(dummie, "yll") <- -59.95
  attr(dummie, "cellsize") <- 0.1
  attr(dummie, "type") <- 'numeric'
  class(dummie) <- "asc"
  dummie <- raster.from.asc(dummie)
  if (i >= 2){
    precip <- stack(precip, dummie)
  } else {
    precip <- dummie      
  }
}