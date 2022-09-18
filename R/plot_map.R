#' Precipitation map plot
#'
#' Function for mapping the first layer of a .nc file
#'
#' @import data.table ggplot2
#' @importFrom grDevices dev.size
#' @importFrom methods as is
#' @importFrom raster as.data.frame
#' @param dummie a .nc file with precipitation
#' @return ggplot object
#' @export

plot_map <- function(dummie){
  if (!is(dummie, "RasterLayer")){
    stop("Error: please plot one Raster layer at the time")
  }
  dummie <- as.data.frame(dummie, xy = TRUE, long = TRUE, na.rm = TRUE)
  dummie <- as.data.table(dummie)
  p00 <- ggplot(dummie, aes(x = x, y = y)) +
    geom_raster(aes(fill = value)) +
    borders(colour = "black") +
    theme_bw() +
    coord_cartesian(xlim = c(min(dummie$x), max(dummie$x)), 
                    ylim = c(min(dummie$y), max(dummie$y))) +
    labs(x = NULL, y = NULL, fill = "[mm]", title = format(as.Date(dummie$Z[1]), "%B %Y")) +
    scale_fill_distiller(palette = "Spectral",
                         breaks = seq(min(dummie$value), max(dummie$value), 
                                      round(max((dummie$value) - min(dummie$value))/8)),
                         guide = guide_colorbar(frame.colour = "black",
                                                ticks.colour = "black")) +
    theme(panel.border = element_rect(colour = "black", size = 2),
          panel.grid = element_blank(), plot.tag = element_text(size = 24), 
          plot.title = element_text(size=28), axis.text = element_text(size = 20), 
          axis.title = element_text(size = 24), 
          legend.text = element_text(size = 20), 
          legend.title = element_text(size = 24), 
          axis.ticks.length = unit(-.25, "cm"),
          legend.key.height = unit(dev.size()[2]/10, "inches"))
  return(p00)
}