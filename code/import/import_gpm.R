library(ncdf4)
library(raster)
library(data.table)
library(dplyr)

year_select <- "2003"
start_date <- paste0(year_select, "-01-01")
end_date <- paste0(year_select, "-12-31")
days_array <- seq(as.Date(start_date), as.Date(end_date), by="days") %>% format("%Y-%m-%d")
days_index <- which(days_array %in% c(start_date, end_date))
days_index <- days_index[1]:days_index[2]
data_folder <- paste0("../data/GPM/", year_select)
all_files <- list.files(data_folder)
for (i in days_index){
  ncin <- paste0(data_folder, "/", all_files[i]) %>% nc_open()
  dummie <- ncvar_get(ncin, 'HQprecipitation')
  if (i >= 2){
    precip <- stack(precip, dummie)
  } else {
    precip <- dummie      
  }
}