library(lubridate)
library(rgdal)
library(gdalUtils)
library(raster)
library(dplyr)
library(ncdf4)


year_select <- "1998"
month_select <- "08"
end_date <- as.Date(paste0(year_select, "-", month_select, "-01"))
days_index <- 1:days_in_month(end_date)
data_folder <- paste0("../data/TRMM_3B43/", year_select)
file_name <- paste0(data_folder,"/3B43.", year_select, month_select, "01.7.HDF")
precip <- get_subdatasets(file_name) %>% stack() %>% t() 
projection(precip) <- "+init=epsg:4326"
extent(precip) <- extent(c(-180, 180, -50, 50))
val <- getValues(precip)
val[val < -9999] <- NA
#val[,1] <- val[,1]*24*as.numeric(days_in_month(end_date)) for mm/month
precip <- setValues(precip, val)