#' Boxplot ggplot
#'
#' Convenient and aesthetic visualization of data in a boxplot.
#'
#' @details
#' If `x` is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#' 
#' If `x` is a filename, it should point to a *.nc file.
#' 
#' `var` is a character string describing the variable to be used for the plot title
#' 
#' `unit` is a character string describing the unit of measurement to be used for the plot title
#' 
#' @import data.table ggplot2
#' @importFrom methods as setGeneric setMethod
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
#' s <- plot_box(r)
#' }

setGeneric("plot_box", function(x, var = 'Precipitation',
                                unit = "mm") standardGeneric("plot_box"))

#' @rdname plot_box
#' @method plot_box Raster

setMethod("plot_box", "Raster",
          function(x, var = "Precipitation", unit = "mm") {
            x <- fldmean(x)
            p00 <- ggplot(x, aes(x = as.factor(month(date)), y = value)) +
              geom_boxplot(fill = "#377eb8") +
              theme_bw() +
              labs(x = NULL, y = paste0(var, ' [', unit, ']'), title = NULL) +
              scale_x_discrete(breaks = seq(1, 12), 
                               labels = c("Jan", "Feb", "Mar", "Apr", "May",
                                          "Jun", "Jul", "Aug", "Sep", "Oct",
                                          "Nov", "Dec")) +
              theme(panel.grid.major.x = element_blank(),
                    plot.title = element_text(size = 24),
                    axis.text = element_text(size = 20),
                    axis.title = element_text(size = 24))
            return(p00)
          })

#' @rdname plot_box
#' @method plot_box data.table

setMethod("plot_box", "data.table",
          function(x, var = "Precipitation", unit = "mm") {
            if (length(unique(x$dataset)) <= 1){
              p00 <- ggplot(x, aes(x = as.factor(month(date)), y = value)) +
                geom_boxplot(fill = '#377eb8')
            } else {
              p00 <- ggplot(x, aes(x = as.factor(month(date)), y = value,
                                   fill = dataset)) +
                geom_boxplot()
            }
            p01 <- p00 +
              theme_bw() +
              labs(x = NULL, y = paste0(var, ' [', unit, ']'), title = NULL) +
              scale_x_discrete(breaks = seq(1, 12), 
                               labels = c("Jan", "Feb", "Mar", "Apr", "May",
                                          "Jun", "Jul", "Aug", "Sep", "Oct",
                                          "Nov", "Dec")) +
              theme(panel.grid.major.x = element_blank(),
                    plot.title = element_text(size = 24),
                    axis.text = element_text(size = 20),
                    axis.title = element_text(size = 24))
            return(p01)
          })

#' @rdname plot_box
#' @method plot_box character

setMethod("plot_box", "character",
          function(x, var = "Precipitation", unit = "mm") {
            x <- brick(x)
            x <- fldmean(x)
            p00 <- ggplot(x, aes(x = as.factor(month(date)), y = value)) +
              geom_boxplot(fill = "#377eb8") +
              theme_bw() +
              labs(x = NULL, y = paste0(var, ' [', unit, ']'), title = NULL) +
              scale_x_discrete(breaks = seq(1, 12), 
                               labels = c("Jan", "Feb", "Mar", "Apr", "May",
                                          "Jun", "Jul", "Aug", "Sep", "Oct",
                                          "Nov", "Dec")) +
              theme(panel.grid.major.x = element_blank(),
                    plot.title = element_text(size = 24),
                    axis.text = element_text(size = 20),
                    axis.title = element_text(size = 24))
            return(p00)
          })
