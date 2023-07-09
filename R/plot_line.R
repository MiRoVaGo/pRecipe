#' Line ggplot
#'
#' Convenient and aesthetic visualization of data in a line plot.
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
#' @importFrom methods setGeneric setMethod
#' @importFrom raster brick
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
#' s <- plot_line(r)
#' }

setGeneric("plot_line",
           function(x,
                    var = "Precipitation",
                    unit = "mm") standardGeneric("plot_line"))

#' @rdname plot_line
#' @method plot_line Raster

setMethod("plot_line", "Raster",
          function(x, var = "Precipitation", unit = "mm") {
            x <- fldmean(x)
            p00 <- ggplot(x, aes(x = date, y = value)) +
              geom_line(color = '#377eb8') +
              theme_bw() +
              labs(x = NULL, y = paste0('[', unit, ']'), title = var) +
              scale_x_date(date_labels = "%Y-%m") +
              theme(plot.title = element_text(size = 24),
                    axis.text = element_text(size = 20),
                    axis.title = element_text(size = 24))
            return(p00)
          })

#' @rdname plot_line
#' @method plot_line data.table

setMethod("plot_line", "data.table",
          function(x, var = "Precipitation", unit = "mm") {
            if (length(unique(x$dataset)) <= 1){
              p00 <- ggplot(x, aes(x = date, y = value)) +
                geom_line(color = '#377eb8')
            } else {
              p00 <- ggplot(x, aes(x = date, y = value, color = dataset)) +
                geom_line()
            }
            p01 <- p00 +
              theme_bw() +
              labs(x = NULL, y = paste0('[', unit, ']'), title = var) +
              scale_x_date(date_labels = "%Y-%m") +
              theme(plot.title = element_text(size = 24),
                    axis.text = element_text(size = 20),
                    axis.title = element_text(size = 24))
            return(p01)
          })

#' @rdname plot_line
#' @method plot_line character

setMethod("plot_line", "character",
          function(x, var = "Precipitation", unit = "mm") {
            x <- brick(x)
            x <- fldmean(x)
            p00 <- ggplot(x, aes(x = date, y = value)) +
              geom_line(color = '#377eb8') +
              theme_bw() +
              labs(x = NULL, y = paste0('[', unit, ']'), title = var) +
              scale_x_date(date_labels = "%Y-%m") +
              theme(plot.title = element_text(size = 24),
                    axis.text = element_text(size = 20),
                    axis.title = element_text(size = 24))
            return(p00)
          })
