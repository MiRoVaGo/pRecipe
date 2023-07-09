#' Taylor diagram
#'
#' Convenient and aesthetic visualization of data in a Taylor diagram.
#' 
#' @details
#' `x` columns should be named: "lon", "lat", "date", "value", "dataset", and "source". The last two columns are added using the \code{\link{label}}.
#' 
#' `y` columns should be named: "lon", "lat", "date", "value", "dataset", and "source". The last two columns are added using the \code{\link{label}}.
#' 
#' `groups` character to define panels. Suitable options are:
#' \itemize{
#' \item "source" (default)
#' \item "seasons" (only works properly with monthly data)
#' }
#' 
#' `...` extra arguments passed on to \code{openair::\link[openair:TaylorDiagram]{TaylorDiagram}}
#'
#' @import data.table
#' @importFrom openair TaylorDiagram
#' @importFrom scales hue_pal
#' @param x data.table
#' @param y data.table
#' @param groups character
#' @param ... see details
#' @return plot object
#' @export

plot_taylor <- function(x, y, groups = "source", ...){
  text_obs <- y$dataset[1]
  y <- y[, .(obs = value), .(date)]
  precip <- merge(y, x, by = "date")
  precip[month(date) == 1 | month(date) == 2 | month(date) == 12,
         seasons := "Winter DJF"
         ][month(date) == 3 | month(date) == 4 | month(date) == 5, 
           seasons := "Spring MAM"
           ][month(date) == 6 | month(date) == 7 | month(date) == 8,
             seasons := "Summer JJA"
             ][month(date) == 9 | month(date) == 10 | month(date) == 11,
               seasons := "Fall SON"]
  precip$seasons <- factor(precip$seasons, levels = c("Spring MAM",
                                                      "Summer JJA",
                                                      "Fall SON",
                                                      "Winter DJF"))
  p00 <- TaylorDiagram(precip, obs = "obs", mod = "value", group = "dataset", 
                       type = groups, xlab = NULL, annotate = "CRMSE",
                       normalise = TRUE, auto.text = FALSE,
                       ylab = "Standard Deviation (Normalized)",
                       key.title = "Data Set",
                       text.obs = text_obs, ...)
  return(p00)
}