#' Crop precipitation data sets
#'
#' The function \code{crop_data} crops the data sets using a shapefile mask and stores it in the same location of the input file.
#'
#' @importFrom raster brick mask
#' @importFrom R.utils getAbsolutePath
#' @importFrom sf read_sf
#' @param data_file a character string with the path to the data file.
#' @param shp_path a character string with the path to the ".shp" file.
#' @return No return value, called to crop and store store the new data file.
#' @export
#' @examples
#' \dontrun{
#' crop_data("gpcp_tp_mm_global_197901_202205_025_monthly.nc", "cze.shp")
#' crop_data("dummie.nc", "cze.shp")
#' }

crop_data <- function(data_file, shp_path){
  shp_mask <- read_sf(shp_path)
  nc_in <- getAbsolutePath(data_file)
  checker <- name_check(data_file)
  if (checker$length == 8) {
    checker$name[4] <- "cropped"
    nc_out <- paste(checker$name, collapse = "_")
    nc_out <- paste0(nc_out, ".nc")
    nc_mid <- sub("(.*/)(.*)", "\\1", nc_in)
    nc_out <- paste0(nc_mid, nc_out)
  } else {
    warning("This is not pRecipe data")
    nc_out <- sub(".nc.*", "", nc_in)
    nc_out <- paste0(nc_out, "_cropped.nc")
  }
  nc_out <- sub(".nc.nc.*", ".nc", nc_out)
  check_out <- exists_check(nc_out)
  if (check_out$exists) stop(check_out$sms)
  dummie_brick <- brick(nc_in)
  dummie_crop <- mask(dummie_brick, shp_mask)
  save_nc(dummie_crop, nc_out)
  return(invisible())
}