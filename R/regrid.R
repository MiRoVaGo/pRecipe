#' Spatial aggregation
#'
#' The function \code{regrid} aggregates data into a new grid resolution.
#'
#' @details
#' If x is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#' 
#' If x is a filename, it should point to a *.nc file.
#' 
#' @import data.table
#' @importFrom methods setGeneric setMethod
#' @importFrom raster aggregate brick getZ raster res setZ
#' @importFrom sp coordinates<- gridded<-
#' @param x Raster* object; data.table (see details); filename (character; see details)
#' @param res numeric. Target resolution must be a multiple of 0.25 (e.g., 0.5, 1, 2.5).
#' @return Raster* object; data.table
#' @export
#' @examples
#' \dontrun{
#' x <- regrid("gpcp_tp_mm_global_197901_202205_025_monthly.nc", 1)
#' z <- regrid("dummie.nc", 1)
#' }

regrid <- function(x, new_res, autosave = FALSE){
  if (new_res%%0.25 != 0) {
    stop("Error: New resolution must be a multiple of 0.25")
  }
  nc_in <- getAbsolutePath(x)
  checker <- name_check(x)
  if (checker$length == 8) {
    if (new_res < 1) {
      checker$name[7] <- sub("\\.", "", new_res)
    } else {
      checker$name[7] <- sub("\\.", "dot", new_res)
    }
    nc_out <- paste(checker$name, collapse = "_")
    nc_out <- paste0(nc_out, ".nc")
    nc_mid <- sub("(.*/)(.*)", "\\1", nc_in)
    nc_out <- paste0(nc_mid, nc_out)
  } else {
    nc_out <- sub(".nc.*", "", nc_in)
    nc_out <- paste0(nc_out, "_aggregated.nc")
  }
  nc_out <- sub(".nc.nc.*", ".nc", nc_out)
  check_out <- exists_check(nc_out)
  if (check_out$exists) stop(check_out$sms)
  if (is.character(x)){
    dummie_aggregated <- aggregate_brick(nc_in, new_res)
  } else {
    dummie_aggregated <- aggregate_brick(x, new_res)
  }
  if (autosave){
    saveNC(dummie_aggregated, nc_out)
    return(invisible())
  } else {
    return(dummie_aggregated)
  }
}