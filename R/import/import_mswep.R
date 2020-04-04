library(ncdf4)
library(raster)
library(data.table)
library(dplyr)

year_select <- "2010"
start_date <- paste0(year_select, "-01-01")
end_date <- paste0(year_select, "-12-31")
days_array <- seq(as.Date(start_date), as.Date(end_date), by="days") %>% format("%Y-%m-%d")
days_index <- which(days_array %in% c(start_date,end_date))
nc_name <- paste0(year_select, ".nc")
ncin <- paste0("../data/MSWEP/", nc_name) %>% nc_open()
precip <- ncvar_get(ncin, 'precipitation', start = c(1, 1, days_index[1]), count = c(-1, -1, days_index[2])) %>% brick()