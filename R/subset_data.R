#' Subset data in space and time
#'
#' The function \code{subset_data} subsets the data in space within a bounding box, and in time within a year range.
#'
#' @details
#' If x is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#' 
#' If x is a filename, it should point to a *.nc file.
#' 
#' @import data.table
#' @importFrom methods setGeneric setMethod
#' @importFrom raster brick
#' @param x Raster* object; data.table (see details); filename (character; see details)
#' @param box numeric. Bounding box in the form: (xmin, xmax, ymin, ymax)
#' @param yrs numeric. Time range in the form: (start_year, end_year)
#' @return Raster* object; data.table
#' @export
#' @examples
#' \dontrun{
#' download_data("gldas-vic", tempdir(), timestep = "yearly")
#' r <- raster::brick(paste0(tempdir(),
#' "/gldas-vic_tp_mm_land_194801_201412_025_yearly.nc"))
#' s <- subset_data(r, c(12.24, 18.85, 48.56, 51.12), c(2000, 2010))
#' }

setGeneric("subset_data", function(x, box, yrs) standardGeneric("subset_data"))

#' @rdname subset_data
#' @method subset_data Raster

setMethod("subset_data", "Raster",
          function(x, box, yrs) {
            dummie_sub <- sellonlatbox(x, box)
            dummie <- selyear(dummie_sub, yrs)
            return(dummie)
          })

#' @rdname subset_data
#' @method subset_data data.table

setMethod("subset_data", "data.table",
          function(x, box, yrs) {
            dummie_sub <- sellonlatbox(x, box)
            dummie <- selyear(dummie_sub, yrs)
            return(dummie)
          })

#' @rdname subset_data
#' @method subset_data character

setMethod("subset_data", "character",
          function(x, box, yrs) {
            dummie_sub <- sellonlatbox(x, box)
            dummie <- selyear(dummie_sub, yrs)
            return(dummie)
          })
