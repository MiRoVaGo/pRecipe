library(ncdf4)
library(raster)
library(data.table)
library(dplyr)

year_select <- "2003"
ncin <- paste0("../data/20CRv2/prate.", year_select, ".nc") %>% nc_open()
precip <- ncvar_get(ncin, 'prate') *86400 %>% brick()