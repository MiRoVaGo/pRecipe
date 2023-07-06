#' Crop precipitation data sets
#'
#' The function \code{crop_data} crops the data sets using a shapefile mask.
#' 
#' @details
#' If x is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#' 
#' If x is a filename, it should point to a *.nc file.
#'
#' @import data.table
#' @import sp
#' @importFrom methods as setGeneric setMethod
#' @importFrom raster brick crop extent mask
#' @importFrom sf read_sf st_bbox
#' @param x Raster* object; data.table (see details); filename (character; see details)
#' @param y filename (character). Path to a *.shp file.
#' @return Raster* object; data.table
#' @export
#' @examples
#' \dontrun{
#' crop_data("gpcp_tp_mm_global_197901_202205_025_monthly.nc", "cze.shp",
#' autosave = TRUE)
#' crop_data("dummie.nc", "cze.shp", autosave = TRUE)
#' }

setGeneric("crop_data", function(x, y) standardGeneric("crop_data"))

#' @rdname crop_data
#' @method crop_data Raster

setMethod("crop_data", "Raster",
          function(x, y) {
            shp <- read_sf(y)
            dummie <- crop(x, shp, snap = "out")
            dummie <- mask(dummie, shp)
            return(dummie)
          })

#' @rdname crop_data
#' @method crop_data data.table

setMethod("crop_data", "data.table",
          function(x, y) {
            shp <- read_sf(y)
            lonlatbox <- st_bbox(shp)
            dummie <- x[(lon >= lonlatbox[1] - 1) & (lon <= lonlatbox[3] + 1) &
                          (lat >= lonlatbox[2] - 1) & (lat <= lonlatbox[4] + 1)]
            coordinates(dummie) <- ~ lon + lat
            proj4string(dummie) <- proj4string(shp)
            dummie <- dummie[!is.na(over(dummie, as(shp, "SpatialPolygons"))), ]
            dummie <- as.data.table(dummie)
            return(dummie)
          })

#' @rdname crop_data
#' @method crop_data character

setMethod("crop_data", "character",
          function(x, y) {
            shp <- read_sf(y)
            dummie_brick <- brick(x)
            dummie <- crop(dummie_brick, shp, snap = "out")
            dummie <- mask(dummie, shp)
            return(dummie)
          })
