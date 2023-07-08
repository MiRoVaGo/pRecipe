#' Histogram ggplot
#'
#' Convenient and aesthetic visualization of data in a histogram.
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
#' @import data.table ggplot2
#' @importFrom methods as setGeneric setMethod
#' @importFrom raster brick
#' @importFrom stats density
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
#' s <- plot_density(r)
#' }

setGeneric("plot_density",
           function(x,
                    var = 'Precipitation',
                    unit = "mm") standardGeneric("plot_density"))

#' @rdname plot_density
#' @method plot_density Raster

setMethod("plot_density", "Raster",
          function(x, var = "Precipitation", unit = "mm") {
            x <- fldmean(x)
            p00 <- ggplot(x, aes(x = value, after_stat(density))) +
              geom_histogram(color = "gray", fill = "#377eb8") +
              geom_density(color = "black", size = 1, linetype = "dashed") +
              theme_bw() +
              labs(y = "Density", x = paste0(var, ' [', unit, ']')) + 
              theme(plot.title = element_text(size = 24),
                    axis.text = element_text(size = 20),
                    axis.title = element_text(size = 24))
            return(p00)
          })

#' @rdname plot_density
#' @method plot_density data.table

setMethod("plot_density", "data.table",
          function(x, var = "Precipitation", unit = "mm") {
            if (length(unique(x$dataset)) <= 1){
              p00 <- ggplot(x, aes(x = value, after_stat(density))) +
                geom_histogram(color = "gray", fill = "#377eb8")
            } else {
              p00 <- ggplot(x, aes(x = value, after_stat(density),
                                   fill = dataset)) +
                geom_histogram(color = "gray")
            }
            p01 <- p00 +
              geom_density(color = "black", size = 1, linetype = "dashed") +
              theme_bw() +
              labs(y = "Density", x = paste0(var, ' [', unit, ']')) + 
              theme(plot.title = element_text(size = 24),
                    axis.text = element_text(size = 20),
                    axis.title = element_text(size = 24))
            return(p01)
          })

#' @rdname plot_density
#' @method plot_density character

setMethod("plot_density", "character",
          function(x, var = "Precipitation", unit = "mm") {
            x <- brick(x)
            x <- fldmean(x)
            p00 <- ggplot(x, aes(x = value, after_stat(density))) +
              geom_histogram(color = "gray", fill = "#377eb8") +
              geom_density(color = "black", size = 1, linetype = "dashed") +
              theme_bw() +
              labs(y = "Density", x = paste0(var, ' [', unit, ']')) + 
              theme(plot.title = element_text(size = 24),
                    axis.text = element_text(size = 20),
                    axis.title = element_text(size = 24))
            return(p00)
          })
