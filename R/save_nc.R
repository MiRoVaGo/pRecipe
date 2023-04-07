#' Save .nc file
#'
#' Function to save data compatible with pRecipe in .nc file
#'
#' @import ncdf4
#' @importFrom methods as is
#' @importFrom raster getValues getZ xFromCol yFromRow
#' @param dummie_nc a Raster object.
#' @param nc_out a character string.
#' @return No return value, called to save a file
#' @export
#' @examples
#' \dontrun{
#' save_nc(dummie_brick, "gpcp_tp_mm_global_197901_202205_025_monthly.nc")
#' }

save_nc <- function(dummie_nc, nc_out){
  lon <- xFromCol(dummie_nc)
  lon <- round(lon, 4)
  lat <- yFromRow(dummie_nc)
  lat <- round(lat, 4)
  time <- getZ(dummie_nc)
  if (is.character(time) | is.numeric(time)) {
    if (is.numeric(time)) {
      time <- as.character(time)
    }
    if (length(time[time == "00"]) >= 1) {
      time <- sub("^00$", "", time)
      time <- time[time != ""]
      time <- as.Date(time)
    } else if (!Reduce("|",grepl("-01", time))) {
      time <- as.numeric(time)
      dummie_origin <- "1970-01-01 00:00:00"
      time <- as.Date(time, origin = dummie_origin)
    } else {
      time <- as.Date(time)
    }
  }
  tp <- getValues(dummie_nc)
  tp[is.na(tp)] <- -9999
  deflon <- ncdim_def("lon", vals = lon, longname = "longitude",
                      units = "degrees_east")
  deflat <- ncdim_def("lat", vals = lat, longname = "latitude",
                      units = "degrees_north")
  deftime <- ncdim_def("time", vals = as.numeric(time), longname = "time",
                       units = "days since 1970-01-01 00:00:0.0",
                       calendar = "standard",
                       unlim = TRUE)
  deftp <- ncvar_def(name = "tp", units = "mm", 
                     list(deflon, deflat, deftime), 
                     missval = -9999,
                     compression = 4,
                     longname = "Total monthly precipitation",
                     prec = "float")
  ncoutput <- nc_create(nc_out, list(deftp), force_v4 = TRUE, verbose = FALSE)
  ncvar_put(ncoutput, deftp, tp)
  ncatt_put(ncoutput,"lon","axis","X") 
  ncatt_put(ncoutput,"lat","axis","Y")
  ncatt_put(ncoutput,"time","axis","T")
  nc_close(ncoutput)
}