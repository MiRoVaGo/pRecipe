library(rNOMADS)
library(raster)
library(data.table)
library(dplyr)

year_select <- "1958"
file_name <- paste0("../data/JRA55/fcst_phy2m125.061_tprat.",year_select, "01_", year_select, "12")
precip <- ReadGrib(file_name)