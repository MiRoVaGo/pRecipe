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
#' `layer` is the layer number to be plotted.
#' 
#' `timestamp` if TRUE (default) the plot title is the layer's date
#'
#' @import data.table ggplot2
#' @importFrom grDevices dev.size
#' @importFrom methods as setGeneric setMethod
#' @importFrom raster as.data.frame brick
#' @importFrom stats quantile
#' @param x Raster* object; data.table (see details); filename (character, see details)
#' @param layer numeric
#' @param unit character
#' @param timestamp logical
#' @return ggplot object
#' @export

setGeneric("plot_map", function(x, layer = 1, unit = "mm", timestamp = TRUE) standardGeneric("plot_map"))

#' @rdname plot_map
#' @method plot_map Raster

setMethod("plot_map", "Raster",
          function(x, layer = 1, unit = "mm", timestamp = TRUE) {
            x <- x[[layer]]
            x <- as.data.frame(x, xy = TRUE, long = TRUE, na.rm = TRUE) %>%
              as.data.table()
            setnames(x, c("lon", "lat", "date", "value"))
            if (round((max(x$lon) - min(x$lon))) == 360){
              map_expand <- FALSE
            } else {
              map_expand <- TRUE
            }
            map_max <- quantile(x$value, 0.9995)
            x[value > map_max, value := map_max]
            if (timestamp == TRUE) {
              date_title <- format(as.Date(x$date[1]), "%B %Y")
            } else {
              date_title <- NULL
            }
            p00 <- ggplot(x, aes(x = lon, y = lat)) +
              geom_raster(aes(fill = value)) +
              borders(colour = "gray23") +
              theme_bw() +
              coord_cartesian(xlim = c(min(x$lon), max(x$lon)), 
                              ylim = c(min(x$lat), max(x$lat)),
                              expand = map_expand) +
              labs(x = NULL, y = NULL, fill = paste0('[', unit, ']'),
                   title = date_title) +
              scale_fill_distiller(palette = "YlGnBu",
                                   direction = 1,
                                   guide = guide_colorbar(frame.colour = "gray23",
                                                          ticks.colour = "gray23")) +
              theme(panel.border = element_rect(colour = "gray23",
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
          function(x, layer = 1, unit = "mm", timestamp = TRUE) {
            dummie_dates <- sort(unique(x$date))
            x <- x[date == dummie_dates[layer]]
            if (round((max(x$lon) - min(x$lon))) == 360){
              map_expand <- FALSE
            } else {
              map_expand <- TRUE
            }
            map_max <- quantile(x$value, 0.9995)
            x[value > map_max, value := map_max]
            if (timestamp == TRUE) {
              date_title <- format(as.Date(x$date[1]), "%B %Y")
            } else {
              date_title <- NULL
            }
            p00 <- ggplot(x, aes(x = lon, y = lat)) +
              geom_raster(aes(fill = value)) +
              borders(colour = "gray23") +
              theme_bw() +
              coord_cartesian(xlim = c(min(x$lon), max(x$lon)), 
                              ylim = c(min(x$lat), max(x$lat)),
                              expand = map_expand) +
              labs(x = NULL, y = NULL, fill = paste0('[', unit, ']'),
                   title = date_title) +
              scale_fill_distiller(palette = "YlGnBu",
                                   direction = 1,
                                   guide = guide_colorbar(frame.colour = "gray23",
                                                          ticks.colour = "gray23")) +
              theme(panel.border = element_rect(colour = "gray23",
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
          function(x, layer = 1, unit = "mm", timestamp = TRUE) {
            x <- brick(x)
            x <- x[[layer]]
            x <- as.data.frame(x, xy = TRUE, long = TRUE, na.rm = TRUE) %>%
              as.data.table()
            setnames(x, c("lon", "lat", "date", "value"))
            if (round((max(x$lon) - min(x$lon))) == 360){
              map_expand <- FALSE
            } else {
              map_expand <- TRUE
            }
            map_max <- quantile(x$value, 0.9995)
            x[value > map_max, value := map_max]
            if (timestamp == TRUE) {
              date_title <- format(as.Date(x$date[1]), "%B %Y")
            } else {
              date_title <- NULL
            }
            p00 <- ggplot(x, aes(x = lon, y = lat)) +
              geom_raster(aes(fill = value)) +
              borders(colour = "gray23") +
              theme_bw() +
              coord_cartesian(xlim = c(min(x$lon), max(x$lon)), 
                              ylim = c(min(x$lat), max(x$lat)),
                              expand = map_expand) +
              labs(x = NULL, y = NULL, fill = paste0('[', unit, ']'),
                   title = date_title) +
              scale_fill_distiller(palette = "YlGnBu",
                                   direction = 1,
                                   guide = guide_colorbar(frame.colour = "gray23",
                                                          ticks.colour = "gray23")) +
              theme(panel.border = element_rect(colour = "gray23",
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
