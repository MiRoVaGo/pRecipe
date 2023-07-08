#' Subset data in space and time
#'
#' The function \code{subset_data} subsets the data in space within a bounding box, and/or in time within a year range.
#'
#' @details
#' If `x` is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#' 
#' If `x` is a filename, it should point to a *.nc file.
#' 
#' If subsetting only in space or time then the arguments must be passed by name. I.e., \code{subset_data(x, box = ...)} (space) or \code{subset_data(x, yrs = ...)} (time)
#' 
#' @importFrom methods is setGeneric setMethod
#' @param x Raster* object; data.table (see details); filename (character, see details)
#' @param box numeric. Bounding box in the form: (xmin, xmax, ymin, ymax)
#' @param yrs numeric. Time range in the form: (start_year, end_year)
#' @return Raster* object; data.table
#' @export
#' @examples
#' \dontrun{
#' download_data("gldas-vic", tempdir(), timestep = "yearly")
#' r <- raster::brick(paste0(tempdir(),
#' "/gldas-vic_tp_mm_land_194801_201412_025_yearly.nc"))
#' sd <- subset_data(r, c(12.24, 18.85, 48.56, 51.12), c(2000, 2010))
#' ss <- subset_data(r, box = c(12.24, 18.85, 48.56, 51.12))
#' st <- subset_data(r, yrs = c(2000, 2010))
#' }

setGeneric("subset_data", function(x, box = NULL, yrs = NULL) standardGeneric("subset_data"))

#' @rdname subset_data
#' @method subset_data Raster

setMethod("subset_data", "Raster",
          function(x, box = NULL, yrs = NULL) {
            if (!is.null(box)) {
              dummie <- sellonlatbox(x, box)
            } else {
              dummie <- x
            }
            if (!is.null(yrs)) {
              dummie <- selyear(dummie, yrs)
            }
            return(dummie)
          })

#' @rdname subset_data
#' @method subset_data data.table

setMethod("subset_data", "data.table",
          function(x, box = NULL, yrs = NULL) {
            if (!is.null(box)) {
              dummie <- sellonlatbox(x, box)
            } else {
              dummie <- x
            }
            if (!is.null(yrs)) {
              dummie <- selyear(dummie, yrs)
            }
            return(dummie)
          })

#' @rdname subset_data
#' @method subset_data character

setMethod("subset_data", "character",
          function(x, box = NULL, yrs = NULL) {
            if (!is.null(box)) {
              dummie <- sellonlatbox(x, box)
            } else {
              dummie <- x
            }
            if (!is.null(yrs)) {
              dummie <- selyear(dummie, yrs)
            }
            return(dummie)
          })
