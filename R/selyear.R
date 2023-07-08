#' Select Years
#'
#' The function \code{selyear} subsets the data in time within a year range.
#'
#' @details
#' If `x` is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#' 
#' If `x` is a filename, it should point to a *.nc file.
#' 
#' @import data.table
#' @importFrom methods is setGeneric setMethod
#' @importFrom raster brick getZ setZ subset
#' @param x Raster* object; data.table (see details); filename (character, see details)
#' @param y numeric. Time range in the form: (start_year, end_year)
#' @return Raster* object; data.table
#' @keywords internal
#' @examples
#' \dontrun{
#' download_data("gldas-vic", tempdir(), timestep = "yearly")
#' r <- raster::brick(paste0(tempdir(),
#' "/gldas-vic_tp_mm_land_194801_201412_025_yearly.nc"))
#' s <- selyear(r, c(2000, 2010))
#' }

setGeneric("selyear", function(x, y) standardGeneric("selyear"))

#' @rdname selyear
#' @method selyear Raster

setMethod("selyear", "Raster",
          function(x, y) {
            start_year <- paste0(y[1], "-01-01")
            final_year <- paste0(y[2], "-12-31")
            old_dates <- getZ(x) %>% aux_date()
            range_years <- which((old_dates >= start_year) &
                                   (old_dates <= final_year))
            dummie <- subset(x, range_years)
            new_dates <- old_dates[range_years]
            dummie <- setZ(dummie, new_dates)
            if (is(dummie, "RasterStack")) {
              dummie <- brick(dummie)
            }
            dummie <- setZ(dummie, new_dates)
            return(dummie)
          })

#' @rdname selyear
#' @method selyear data.table

setMethod("selyear", "data.table",
          function(x, y) {
            dummie <- x[(year(date) >= y[1]) & (year(date) <= y[2])]
            return(dummie)
          })

#' @rdname selyear
#' @method selyear character

setMethod("selyear", "character",
          function(x, y) {
            dummie_brick <- brick(x)
            start_year <- paste0(y[1], "-01-01")
            final_year <- paste0(y[2], "-12-31")
            old_dates <- getZ(dummie_brick) %>% aux_date()
            range_years <- which((old_dates >= start_year) &
                                   (old_dates <= final_year))
            dummie <- subset(dummie_brick, range_years)
            new_dates <- old_dates[range_years]
            dummie <- setZ(dummie, new_dates)
            if (is(dummie, "RasterStack")) {
              dummie <- brick(dummie)
            }
            dummie <- setZ(dummie, new_dates)
            return(dummie)
          })
