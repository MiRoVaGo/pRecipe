library(ncdf4)
library(raster)
library(data.table)
library(dplyr)

year_select <- "2003"
ncin <- paste0("../data/ERA_Interim/prate.", year_select, ".nc") %>% nc_open()
precip <- ncvar_get(ncin, 'tp') %>% brick()