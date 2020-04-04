library(ncdf4)
library(raster)
library(data.table)
library(dplyr)

start_month <- "1992-01"
end_month <- "1992-12"
month_array <- seq(as.Date("1900-01-01"), as.Date("2015-05-01"), by="months") %>% format("%Y-%m")
month_index <- which(month_array %in% c(start_month,end_month))
month_index[2] <- month_index[2] - month_index[1] + 1
ncin <- nc_open("../data/GHCN/precip.mon.total.nc")
precip <- ncvar_get(ncin, 'precip', start = c(1, 1, month_index[1]), count = c(-1, -1, month_index[2])) %>% brick()