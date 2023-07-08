#' Heatmap ggplot
#'
#' Convenient and aesthetic visualization of data in a heatmap.
#'
#' @details
#' If `x` is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#' 
#' If `x` is a filename, it should point to a *.nc file.
#' 
#' `unit` is a character string describing the unit of measurement to be used for the axis title
#'
#' @import data.table ggplot2
#' @importFrom methods setGeneric setMethod
#' @importFrom raster brick
#' @param x Raster* object; data.table (see details); filename (character, see details)
#' @param unit character (see details)
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#' download_data("gldas-vic", tempdir(), timestep = "yearly")
#' r <- raster::brick(paste0(tempdir(),
#' "/gldas-vic_tp_mm_land_194801_201412_025_yearly.nc"))
#' s <- plot_heatmap(r)
#' }

setGeneric("plot_heatmap",
           function(x, unit = "mm") standardGeneric("plot_heatmap"))

#' @rdname plot_heatmap
#' @method plot_density Raster

setMethod("plot_heatmap", "Raster",
          function(x, unit = "mm") {
            x <- fldmean(x)
            p00 <- ggplot(x, aes(x = year(date), y = month(date))) + 
              geom_raster(aes(fill = value)) +
              scale_x_continuous(breaks = seq(min(year(x$date)),
                                              max(year(x$date))),
                                 expand = c(0,0)) +
              scale_y_reverse(breaks = seq(1, 12),
                              labels = c("Jan", "Feb", "Mar", "Apr", "May",
                                         "Jun", "Jul", "Aug", "Sep", "Oct",
                                         "Nov", "Dec"),
                              expand = c(0,0)) +
              theme_bw() +
              scale_fill_distiller(palette = "YlGnBu",
                                   direction = 1,
                                   guide = guide_colorbar(frame.colour = "black",
                                                          ticks.colour = "black")) +
              coord_equal() +
              labs(x = NULL, y = NULL, fill = paste0('[', unit, ']')) +
              theme(panel.grid = element_blank(), 
                    axis.text.x = element_text(angle = 90, vjust = 0.5,
                                               hjust = 1),
                    panel.background = element_rect(fill = "gray"),
                    plot.title = element_text(size = 22),
                    axis.text = element_text(size = 18),
                    axis.title = element_text(size = 22))
            return(p00)
          })

#' @rdname plot_heatmap
#' @method plot_density data.table

setMethod("plot_heatmap", "data.table",
          function(x, unit = "mm") {
            p00 <- ggplot(x, aes(x = year(date), y = month(date))) + 
              geom_raster(aes(fill = value)) +
              scale_x_continuous(breaks = seq(min(year(x$date)),
                                              max(year(x$date))),
                                 expand = c(0,0)) +
              scale_y_reverse(breaks = seq(1, 12),
                              labels = c("Jan", "Feb", "Mar", "Apr", "May",
                                         "Jun", "Jul", "Aug", "Sep", "Oct",
                                         "Nov", "Dec"),
                              expand = c(0,0)) +
              theme_bw() +
              scale_fill_distiller(palette = "YlGnBu",
                                   direction = 1,
                                   guide = guide_colorbar(frame.colour = "black",
                                                          ticks.colour = "black")) +
              coord_equal() +
              labs(x = NULL, y = NULL, fill = paste0('[', unit, ']')) +
              theme(panel.grid = element_blank(), 
                    axis.text.x = element_text(angle = 90, vjust = 0.5,
                                               hjust = 1),
                    panel.background = element_rect(fill = "gray"),
                    plot.title = element_text(size = 22),
                    axis.text = element_text(size = 18),
                    axis.title = element_text(size = 22))
            return(p00)
          })

#' @rdname plot_heatmap
#' @method plot_density character

setMethod("plot_heatmap", "character",
          function(x, unit = "mm") {
            x <- brick(x)
            x <- fldmean(x)
            p00 <- ggplot(x, aes(x = year(date), y = month(date))) + 
              geom_raster(aes(fill = value)) +
              scale_x_continuous(breaks = seq(min(year(x$date)),
                                              max(year(x$date))),
                                 expand = c(0,0)) +
              scale_y_reverse(breaks = seq(1, 12),
                              labels = c("Jan", "Feb", "Mar", "Apr", "May",
                                         "Jun", "Jul", "Aug", "Sep", "Oct",
                                         "Nov", "Dec"),
                              expand = c(0,0)) +
              theme_bw() +
              scale_fill_distiller(palette = "YlGnBu",
                                   direction = 1,
                                   guide = guide_colorbar(frame.colour = "black",
                                                          ticks.colour = "black")) +
              coord_equal() +
              labs(x = NULL, y = NULL, fill = paste0('[', unit, ']')) +
              theme(panel.grid = element_blank(), 
                    axis.text.x = element_text(angle = 90, vjust = 0.5,
                                               hjust = 1),
                    panel.background = element_rect(fill = "gray"),
                    plot.title = element_text(size = 22),
                    axis.text = element_text(size = 18),
                    axis.title = element_text(size = 22))
            return(p00)
          })
