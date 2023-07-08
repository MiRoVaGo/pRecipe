#' Map ggplot
#'
#' Convenient and aesthetic visualization of data in a map
#' 
#' @details
#' If `x` is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#' 
#' If `x` is a filename, it should point to a *.nc file.
#' 
#' `unit` is a character string describing the unit of measurement to be used for the legend title
#'
#' @import data.table ggplot2
#' @importFrom grDevices dev.size
#' @importFrom methods as setGeneric setMethod
#' @importFrom raster as.data.frame brick
#' @importFrom stats quantile
#' @param x Raster* object; data.table (see details); filename (character, see details)
#' @param unit character
#' @return ggplot object
#' @export

setGeneric("plot_map", function(x, unit = "mm") standardGeneric("plot_map"))

#' @rdname plot_map
#' @method plot_map Raster

setMethod("plot_map", "Raster",
          function(x, unit = "mm") {
            x <- x[[1]]
            x <- as.data.frame(x, xy = TRUE, long = TRUE, na.rm = TRUE) %>%
              as.data.table()
            setnames(x, c("lon", "lat", "date", "value"))
            if (round((max(x$lon) - min(x$lon))) == 360){
              map_expand <- FALSE
            } else {
              map_expand <- TRUE
            }
            map_max <- quantile(x$value, 0.9995)
            map_min <- round(min(x$value)) - 1 
            if (map_min < 0){
              map_min <- 0
            }
            x[value > map_max, value := map_max
              ][value < map_min, value := map_min]
            p00 <- ggplot(x, aes(x = lon, y = lat)) +
              geom_raster(aes(fill = value)) +
              borders(colour = "black") +
              theme_bw() +
              coord_cartesian(xlim = c(min(x$lon), max(x$lon)), 
                              ylim = c(min(x$lat), max(x$lat)),
                              expand = map_expand) +
              labs(x = NULL, y = NULL, fill = paste0('[', unit, ']'),
                   title = format(as.Date(x$date[1]), "%B %Y")) +
              scale_fill_distiller(palette = "YlGnBu",
                                   direction = 1,
                                   limits = c(map_min, map_max),
                                   guide = guide_colorbar(frame.colour = "black",
                                                          ticks.colour = "black")) +
              theme(panel.border = element_rect(colour = "black",
                                                linewidth = 2),
                    panel.grid = element_blank(),
                    plot.tag = element_text(size = 24), 
                    plot.title = element_text(size = 28),
                    axis.text = element_text(size = 20), 
                    axis.title = element_text(size = 24), 
                    legend.text = element_text(size = 20), 
                    legend.title = element_text(size = 24), 
                    axis.ticks.length = unit(-0.25, "cm"),
                    legend.key.height = unit(dev.size()[2]/10, "inches"))
            return(p00)
          })

#' @rdname plot_map
#' @method plot_map data.table

setMethod("plot_map", "data.table",
          function(x, unit = "mm") {
            if (round((max(x$lon) - min(x$lon))) == 360){
              map_expand <- FALSE
            } else {
              map_expand <- TRUE
            }
            map_max <- quantile(x$value, 0.9995)
            map_min <- round(min(x$value)) - 1 
            if (map_min < 0){
              map_min <- 0
            }
            x[value > map_max, value := map_max
              ][value < map_min, value := map_min]
            p00 <- ggplot(x, aes(x = lon, y = lat)) +
              geom_raster(aes(fill = value)) +
              borders(colour = "black") +
              theme_bw() +
              coord_cartesian(xlim = c(min(x$lon), max(x$lon)), 
                              ylim = c(min(x$lat), max(x$lat)),
                              expand = map_expand) +
              labs(x = NULL, y = NULL, fill = paste0('[', unit, ']'),
                   title = format(as.Date(x$date[1]), "%B %Y")) +
              scale_fill_distiller(palette = "YlGnBu",
                                   direction = 1,
                                   limits = c(map_min, map_max),
                                   guide = guide_colorbar(frame.colour = "black",
                                                          ticks.colour = "black")) +
              theme(panel.border = element_rect(colour = "black",
                                                linewidth = 2),
                    panel.grid = element_blank(),
                    plot.tag = element_text(size = 24), 
                    plot.title = element_text(size = 28),
                    axis.text = element_text(size = 20), 
                    axis.title = element_text(size = 24), 
                    legend.text = element_text(size = 20), 
                    legend.title = element_text(size = 24), 
                    axis.ticks.length = unit(-0.25, "cm"),
                    legend.key.height = unit(dev.size()[2]/10, "inches"))
            return(p00)
          })

#' @rdname plot_map
#' @method plot_map character

setMethod("plot_map", "character",
          function(x, unit = "mm") {
            x <- brick(x)
            x <- x[[1]]
            x <- as.data.frame(x, xy = TRUE, long = TRUE, na.rm = TRUE) %>%
              as.data.table()
            setnames(x, c("lon", "lat", "date", "value"))
            if (round((max(x$lon) - min(x$lon))) == 360){
              map_expand <- FALSE
            } else {
              map_expand <- TRUE
            }
            map_max <- quantile(x$value, 0.9995)
            map_min <- round(min(x$value)) - 1 
            if (map_min < 0){
              map_min <- 0
            }
            x[value > map_max, value := map_max
              ][value < map_min, value := map_min]
            p00 <- ggplot(x, aes(x = lon, y = lat)) +
              geom_raster(aes(fill = value)) +
              borders(colour = "black") +
              theme_bw() +
              coord_cartesian(xlim = c(min(x$lon), max(x$lon)), 
                              ylim = c(min(x$lat), max(x$lat)),
                              expand = map_expand) +
              labs(x = NULL, y = NULL, fill = paste0('[', unit, ']'),
                   title = format(as.Date(x$date[1]), "%B %Y")) +
              scale_fill_distiller(palette = "YlGnBu",
                                   direction = 1,
                                   limits = c(map_min, map_max),
                                   guide = guide_colorbar(frame.colour = "black",
                                                          ticks.colour = "black")) +
              theme(panel.border = element_rect(colour = "black",
                                                linewidth = 2),
                    panel.grid = element_blank(),
                    plot.tag = element_text(size = 24), 
                    plot.title = element_text(size = 28),
                    axis.text = element_text(size = 20), 
                    axis.title = element_text(size = 24), 
                    legend.text = element_text(size = 20), 
                    legend.title = element_text(size = 24), 
                    axis.ticks.length = unit(-0.25, "cm"),
                    legend.key.height = unit(dev.size()[2]/10, "inches"))
            return(p00)
          })
