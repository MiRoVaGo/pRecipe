#' Summary ggplot
#'
#' Convenient and aesthetic visualization of data in a summary plot.
#' 
#' @details
#' If `x` is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#' 
#' If `x` is a filename, it should point to a *.nc file.
#' 
#' `var` is a character string describing the variable to be used for the axis title
#' 
#' `unit` is a character string describing the unit of measurement to be used for the axis title
#'
#' @import ggplot2
#' @importFrom ggpubr ggarrange
#' @importFrom methods setGeneric setMethod
#' @param x Raster* object; data.table (see details); filename (character, see details)
#' @param var character (see details)
#' @param unit character (see details)
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#' download_data("gldas-vic", tempdir(), timestep = "yearly")
#' r <- raster::brick(paste0(tempdir(),
#' "/gldas-vic_tp_mm_land_194801_201412_025_yearly.nc"))
#' s <- plot_summary(r)
#' }

setGeneric("plot_summary",
           function(x,
                    var = "Precipitation",
                    unit = "mm") standardGeneric("plot_summary"))

#' @rdname plot_summary
#' @method plot_summary Raster

setMethod("plot_summary", "Raster",
          function(x, var = "Precipitation", unit = "mm") {
            p01 <- plot_line(x, var, unit)
            p02 <- plot_heatmap(x, unit)
            p03 <- plot_box(x, var, unit)
            p04 <- plot_density(x, var, unit)
            p05 <- ggarrange(p01, p02, ncol = 1)
            p06 <- ggarrange(p03, p04, ncol = 2)
            p00 <- ggarrange(p05, p06, ncol = 1, heights = c(2, 1))
            return(p00)
          })

#' @rdname plot_summary
#' @method plot_summary data.table

setMethod("plot_summary", "data.table",
          function(x, var = "Precipitation", unit = "mm") {
            p01 <- plot_line(x, var, unit)
            p02 <- plot_heatmap(x, unit)
            p03 <- plot_box(x, var, unit)
            p04 <- plot_density(x, var, unit)
            p05 <- ggarrange(p01, p02, ncol = 1)
            p06 <- ggarrange(p03, p04, ncol = 2)
            p00 <- ggarrange(p05, p06, ncol = 1, heights = c(2, 1))
            return(p00)
          })

#' @rdname plot_summary
#' @method plot_summary character

setMethod("plot_summary", "character",
          function(x, var = "Precipitation", unit = "mm") {
            p01 <- plot_line(x, var, unit)
            p02 <- plot_heatmap(x, unit)
            p03 <- plot_box(x, var, unit)
            p04 <- plot_density(x, var, unit)
            p05 <- ggarrange(p01, p02, ncol = 1)
            p06 <- ggarrange(p03, p04, ncol = 2)
            p00 <- ggarrange(p05, p06, ncol = 1, heights = c(2, 1))
            return(p00)
          })
