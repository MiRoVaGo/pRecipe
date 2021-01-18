library(data.table)
library(ggplot2)
library(rgdal)
library(viridis)

# plot_functions -----------------------------------------------------------


#' Plot monthly time series
#'
#' Function for plotting monthly time-series of different data-sets and their average as a multiline plot. .
#'
#' @param folder_path a character string with the path where the "data_table" is located.
#' @param name a character string specify the name of the particular "data_table".

plot_timeseries <- function(folder_path, name){
  
  data_table <- readRDS(paste0(folder_path, "/", name, ".Rds"))
  
  mean_all <- data_table[, .(value = mean(value), name = factor("average")), by = c("x", "y", "Z")]
  data_table <- rbind(data_table, mean_all)
  data_table <- data_table[, reg_val := mean(value), by = .(month(Z), year(Z), name)][, ':='(reg_monmean = mean(reg_val), 
                                                                                             reg_monstd = sd(reg_val)), by = .(month(Z), name)]
  
  name_count <- length(unique(data_table$name)) - 1 
  
  ggplot(data_table, aes(Z, reg_val, color = name, linetype = name)) + 
    geom_line() + 
    geom_point() + 
    scale_linetype_manual(values = c(rep("dashed", name_count), "solid")) + 
    labs(x = "Month", y = "Precipitation (mm)") + 
    theme_bw()
  
}

#' Plot seasonality bar plots
#'
#' Function for plotting seasonality of different data-sets as a bar blot along with the error bar of one standard deviation.
#'
#' @param folder_path a character string with the path where the "data-table" is located.
#' @param name a character string specify the name of the particular "data_table".

plot_seasonality <- function(folder_path, name){
  
  data_table <- readRDS(paste0(folder_path, "/", name, ".Rds"))
  
  data_table <- data_table[, reg_val := mean(value), 
                           by = .(month(Z), year(Z), name)][, ':='(reg_monmean = mean(reg_val),
                                                                   reg_monstd = sd(reg_val)), by = .(month(Z), name)]
  
  ggplot(data_table, aes(factor(month(Z)), reg_monmean, fill = name)) + 
    geom_bar(stat="identity", position=position_dodge(), alpha=0.5) + 
    geom_errorbar(aes(ymin = reg_monmean - reg_monstd, 
                      ymax = reg_monmean + reg_monstd), width = .4, 
                  position=position_dodge(width=0.90)) + 
    labs(x = "Month", y = "Precipitation (mm/month)") + 
    theme_bw()
  
}



#' Saptial plots
#'
#' Function for plotting spatial plot of  annual total mean precipitation of different data-sets along with the overlaid shapefile.
#' @param data_table a character string with the path where the "data-table" is located.
#' @param name a character string specify the name of the particular "data_table".
#' @param shapefile_path a character string represents the path to the particular shape_file. It should ends with "/name.shp"

plot_annmean <- function(folder_path, name, shapefile_path){
  
  data_table <- readRDS(paste0(folder_path, "/", name, ".Rds"))
  
  data_table <- data_table[, year_val := sum(value), by = .(year(Z), x, y, name)][, ':='(anl_mean = mean(year_val), 
                                                                                         anl_std = sd(year_val)),  by = .(x, y, name)]
  
  name_shp <- readOGR(shapefile_path)
  
  ggplot(data_table) +
    geom_raster(aes(x, y, fill = anl_mean)) + 
    coord_fixed(ratio = 1) + 
    scale_fill_viridis(direction = -1) + 
    labs(x = "Longitude", y = "Latitude", fill = "mm/yr") + 
    facet_wrap(~name, ncol = 2) + 
    ggtitle("Annual total mean precipitation (mm/yr)") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_path(data = name_shp, 
              aes(x = long, y = lat, group = group))
  
}

#' Spatial plot of standard deviation
#'
#' Function for plotting spatial plot of  standard deviation annual precipitation over the years of different data-sets with overlaid shape-file.
#' @param data_table a character string with the path where the "data-table" is located.
#' @param name a character string specify the name of the particular "data_table".
#' @param shapefile_path a character string represents the path to the particular shape_file. It should ends with "/name.shp"


plot_annstd <- function(folder_path, name, shapefile_path){
  
  data_table <- readRDS(paste0(folder_path, "/", name, ".Rds"))
  
  data_table <- data_table[, year_val := sum(value), by = .(year(Z), x, y, name)][, ':='(anl_mean = mean(year_val), 
                                                                                         anl_std = sd(year_val)),  by = .(x, y, name)]
  name_shp <- readOGR(shapefile_path)
  
  ggplot(data_table) +
    geom_raster(aes(x, y, fill = anl_std)) + 
    coord_fixed(ratio = 1) + 
    scale_fill_viridis(direction = -1) + 
    labs(x = "Longitude", y = "Latitude", fill = "mm/yr") + 
    facet_wrap(~name, ncol = 2) + 
    ggtitle("Annual precipitation standard deviation (mm/yr)") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_path(data = name_shp, 
              aes(x = long, y = lat, group = group))
  
}


# ---------------------------------------------------------------------------


