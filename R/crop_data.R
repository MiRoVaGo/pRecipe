#' Crop precipitation data sets
#'
#' The function \code{crop_data} crops the data sets using a shapefile mask.
#'
#' @import ncdf4
#' @importFrom methods is 
#' @importFrom raster brick getValues mask
#' @importFrom sf read_sf
#' @param nc_path a character string with the path to the ".nc" file or a raster object
#' @param shp_path a character string with the path to the ".shp" file.
#' @param save_nc logical. If TRUE will write a ".nc" file with the cropped data and there will be no return value.
#' @return a raster brick with cropped data
#' @export
#' @examples
#' \dontrun{
#' x <- crop_data("gpm_imerg.nc", "cze.shp")
#' crop_data("gpm_imerg.nc", "cze.shp", TRUE)
#' w <- raster::brick("dummie.nc")
#' z <- crop_data(w, "cze.shp")
#' }

crop_data <- function(nc_path, shp_path, save_nc = FALSE){
  shp_mask <- read_sf(shp_path)
  if (is(nc_path, "RasterBrick") | is(nc_path, "RasterLayer") | is(nc_path, "RasterStack")) {
    tp_file <- nc_path
  } else {
    tp_file <- brick(nc_path)
  }
  x <- mask(tp_file, shp_mask)
  if (save_nc & !is.null(nc_out)){
    nc_in <- nc_open(nc_path)
    lon <- ncvar_get(nc_in, "lon")
    dimlon <- ncatt_get(nc_in, "lon")
    lat <- ncvar_get(nc_in, "lat")
    dimlat <- ncatt_get(nc_in, "lat")
    time <- ncvar_get(nc_in, "time")
    dimtime <- ncatt_get(nc_in, "time")
    tp <- ncvar_get(nc_in, "tp")
    dimtp <- ncatt_get(nc_in, "tp")
    nc_close(nc_in)
    x <- getValues(x)
    tp[is.na(x)] <- -9999
    nc_out <- sub(".nc.*", "", nc_path)
    nc_out <- paste0(nc_out, ".ts.csv")
    deflon <- ncdim_def("lon", vals = lon, longname = dimlon$long_name,
                        units = dimlon$units)
    deflat <- ncdim_def("lat", vals = lat, longname = dimlat$long_name,
                        units = dimlat$units)
    deftime <- ncdim_def("time", vals = time, longname = dimtime$long_name,
                         units = dimtime$units, calendar = dimtime$calendar,
                         unlim = TRUE)
    deftp <- ncvar_def(name = "tp", units = dimtp$units, 
                       list(deflon, deflat, deftime), 
                       missval = dimtp$missing_value,
                       longname = dimtp$long_name, prec = "float")
    ncoutput <- nc_create(nc_out, list(deftp), force_v4 = TRUE, verbose = FALSE)
    ncvar_put(ncoutput, deftp, tp)
    ncatt_put(ncoutput,"lon","axis","X") 
    ncatt_put(ncoutput,"lat","axis","Y")
    ncatt_put(ncoutput,"time","axis","T")
    nc_close(ncoutput)
    return(invisible())
  } else {
    return(x)
  } 
}