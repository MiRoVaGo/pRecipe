#' Multiply by days per month
#'
#' The function \code{muldpm} multiplies the value by days per month.
#' 
#' @details
#' `x` object with monthly data in [units/day]
#' 
#' If `x` is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#' 
#' If `x` is a filename, it should point to a *.nc file.
#' 
#' @import data.table
#' @importFrom methods as setGeneric setMethod
#' @importFrom lubridate days_in_month
#' @importFrom raster brick getZ
#' @param x Raster* object; data.table (see details); filename (character, see details)
#' @return Raster* object; data.table
#' @export
#' @examples
#' \dontrun{
#' tavg_brick <- raster::brick('terraclimate_tavg.nc')
#' pet_od <- pet(method = "od", tavg = tavg_brick)
#' pet_od <- muldpm(pet_od)
#' }

setGeneric("muldpm", function(x) standardGeneric("muldpm"))

#' @rdname muldpm
#' @method muldpm Raster

setMethod("muldpm", "Raster",
          function(x) {
            dummie_dates <- getZ(x) %>% aux_date()
            dummie_days <- as.numeric(days_in_month(dummie_dates))
            dummie <- x*dummie_days
            dummie <- setZ(dummie, dummie_dates)
            return(dummie)
          })

#' @rdname muldpm
#' @method muldpm data.table

setMethod("muldpm", "data.table",
          function(x) {
            dummie <- x[, value := value*as.numeric(days_in_month(date))]
            return(dummie)
          })

#' @rdname muldpm
#' @method muldpm character

setMethod("muldpm", "character",
          function(x) {
            x <- brick(x)
            dummie_dates <- getZ(x) %>% aux_date()
            dummie_days <- as.numeric(days_in_month(dummie_dates))
            dummie <- x*dummie_days
            dummie <- setZ(dummie, dummie_dates)
            return(dummie)
          })
