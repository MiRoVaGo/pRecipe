library(ncdf4)
library(raster)
library(data.table)
library(dplyr)

start_month <- "1992-08"
end_month <- "1993-08"
month_array <- seq(as.Date("1901-01-01"), as.Date("2013-12-01"), by="months") %>% format("%Y-%m")
month_index <- which(month_array %in% c(start_month,end_month))
month_index[2] <- month_index[2] - month_index[1] + 1
ncin <- nc_open("../data/GPCC/full_data_v7_05.nc")
precip <- ncvar_get(ncin, 'p', start = c(1, 1, month_index[1]), count = c(-1, -1, month_index[2])) %>% brick()