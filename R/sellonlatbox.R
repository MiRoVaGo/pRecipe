#' Select Longitude Latitude Box
#'
#' The function \code{sellonlatbox} subsets the data in space within a bounding box.
#' 
#' @details
#' If x is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#' 
#' If x is a filename, it should point to a *.nc file.
#'
#' @import data.table
#' @importFrom methods setGeneric setMethod
#' @importFrom raster brick crop extent
#' @param x Raster* object; data.table (see details); filename (character, see details)
#' @param y numeric. Bounding box in the form: (xmin, xmax, ymin, ymax)
#' @return Raster* object; data.table
#' @export
#' @examples
#' \dontrun{
#' download_data("gldas-vic", tempdir(), timestep = "yearly")
#' r <- raster::brick(paste0(tempdir(),
#' "/gldas-vic_tp_mm_land_194801_201412_025_yearly.nc"))
#' s <- sellonlatbox(r, c(12.24, 18.85, 48.56, 51.12))
#' }

setGeneric("sellonlatbox", function(x, y) standardGeneric("sellonlatbox"))

#' @rdname sellonlatbox
#' @method sellonlatbox Raster

setMethod("sellonlatbox", "Raster",
          function(x, y) {
            lonlatbox <- extent(y[1], y[2], y[3], y[4])
            dummie <- crop(x, lonlatbox)
            return(dummie)
          })

#' @rdname sellonlatbox
#' @method sellonlatbox data.table

setMethod("sellonlatbox", "data.table",
          function(x, y) {
            dummie <- x[(lon >= y[1]) & (lon <= y[2]) &
                                     (lat >= y[3]) & (lat <= y[4])]
            return(dummie)
          })

#' @rdname sellonlatbox
#' @method sellonlatbox character

setMethod("sellonlatbox", "character",
          function(x, y) {
            dummie_brick <- brick(x)
            lonlatbox <- extent(y[1], y[2], y[3], y[4])
            dummie <- crop(dummie_brick, lonlatbox)
            return(dummie)
          })
