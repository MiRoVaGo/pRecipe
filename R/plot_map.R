#' Precipitation map plot
#'
#' Function for mapping the first layer of a .nc file
#'
#' @import data.table ggplot2
#' @importFrom grDevices dev.size
#' @importFrom methods as is
#' @importFrom raster as.data.frame
#' @importFrom stats quantile
#' @param x a .nc file with precipitation
#' @param unit a character string with the variable's unit of measurement to be used for the axis title
#' @return ggplot object
#' @export

plot_map <- function(x, unit = 'mm'){
  if (!is(x, 'data.table')){
    if (!is(x, "RasterLayer")){
      stop("Error: please plot one Raster layer at the time")
    }
    x <- as.data.frame(x, xy = TRUE, long = TRUE, na.rm = TRUE)
    x <- as.data.table(x)
  }
  if (round((max(x$x) - min(x$x))) == 360){
    map_expand <- FALSE
  } else {
    map_expand <- TRUE
  }
  map_max <- quantile(x$value,0.9995)
  map_min <- round(min(x$value)) -1 
  if (map_min < 0){
    map_min <- 0
  }
  p00 <- ggplot(x, aes(x = x, y = y)) +
    geom_raster(aes(fill = value)) +
    borders(colour = "black") +
    theme_bw() +
    coord_cartesian(xlim = c(min(x$x), max(x$x)), 
                    ylim = c(min(x$y), max(x$y)),
                    expand = map_expand) +
    labs(x = NULL, y = NULL, fill = paste0('[', unit, ']'),
         title = format(as.Date(x$Z[1]), "%B %Y")) +
    scale_fill_distiller(palette = "YlGnBu",
                         direction = 1,
                         na.value = "#081D58",
                         limits = c(map_min, map_max),
                         guide = guide_colorbar(frame.colour = "black",
                                                ticks.colour = "black")) +
    theme(panel.border = element_rect(colour = "black", linewidth = 2),
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