library(ncdf4)
library(raster)
library(data.table)
library(dplyr)

ncin <- nc_open("../data/GPCC_1dd/GPCP_1DD_v1.2_199610-201510.nc")
precip <- ncvar_get(ncin, 'PREC') %>% brick()
