library(ncdf4)
library(raster)
library(data.table)
library(dplyr)

start_month <- "1992-08"
end_month <- "2018-08"
month_array <- seq(as.Date("1901-01-01"), as.Date("2018-12-31"), by="months") %>% format("%Y-%m")
month_index <- which(month_array %in% c(start_month,end_month))
month_index[2] <- month_index[2] - month_index[1] + 1
ncin <- nc_open("../data/CRU_TS4_03/cru_ts4.03.1901.2018.pre.dat.nc")
precip <- ncvar_get(ncin, 'pre', start = c(1, 1, month_index[1]), count = c(-1, -1, month_index[2])) %>% brick()