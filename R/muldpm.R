#' Multiply by days per month
#'
#' Function to calculate pet
#' 
#' @importFrom methods as
#' @importFrom lubridate days_in_month
#' @importFrom raster getZ
#' @param x a RasterBrick object with monthly data in [units/day] 
#' @return a RasterBrick object
#' @export
#' @examples
#' \dontrun{
#' tavg_brick <- raster::brick('terraclimate_tavg.nc')
#' pet_od <- pet(method = "od", tavg = tavg_brick)
#' pet_od <- muldpm(pet_od)
#' }

muldpm <- function(x) {
  dummie_dates <- getZ(x)
  dummie_days <- as.numeric(days_in_month(dummie_dates))
  dummie_brick <- x*dummie_days
  dummie_brick <- setZ(dummie_brick, dummie_dates)
  return(dummie_brick)
}
