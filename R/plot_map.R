#' Precipitation map plot
#'
#' Function for mapping the first layer of a .nc file
#'
#' @import data.table ggplot2
#' @importFrom grDevices dev.size
#' @importFrom methods as is
#' @importFrom raster as.data.frame
#' @importFrom stats quantile
#' @param dummie a .nc file with precipitation
#' @return ggplot object
#' @export

plot_map <- function(dummie){
  if (!is(dummie, "RasterLayer")){
    stop("Error: please plot one Raster layer at the time")
  }
  dummie <- as.data.frame(dummie, xy = TRUE, long = TRUE, na.rm = TRUE)
  dummie <- as.data.table(dummie)
  if (round((max(dummie$x) - min(dummie$x))) == 360){
    map_expand <- FALSE
  } else {
    map_expand <- TRUE
  }
  map_max <- quantile(dummie$value,0.9995)
  map_min <- round(min(dummie$value)) -1 
  if (map_min < 0){
    map_min <- 0
  }
  p00 <- ggplot(dummie, aes(x = x, y = y)) +
    geom_raster(aes(fill = value)) +
    borders(colour = "black") +
    theme_bw() +
    coord_cartesian(xlim = c(min(dummie$x), max(dummie$x)), 
                    ylim = c(min(dummie$y), max(dummie$y)),
                    expand = map_expand) +
    labs(x = NULL, y = NULL, fill = "[mm]", title = format(as.Date(dummie$Z[1]), "%B %Y")) +
    scale_fill_distiller(palette = "YlGnBu",
                         direction = 1,
                         na.value = "#081D58",
                         limits = c(map_min, map_max),
                         guide = guide_colorbar(frame.colour = "black",
                                                ticks.colour = "black")) +
    theme(panel.border = element_rect(colour = "black", size = 2),
          panel.grid = element_blank(), plot.tag = element_text(size = 24), 
          plot.title = element_text(size = 28),
          axis.text = element_text(size = 20), 
          axis.title = element_text(size = 24), 
          legend.text = element_text(size = 20), 
          legend.title = element_text(size = 24), 
          axis.ticks.length = unit(-0.25, "cm"),
          legend.key.height = unit(dev.size()[2]/10, "inches"))
  return(p00)
}