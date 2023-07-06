#' Crop precipitation data sets
#'
#' The function \code{crop_data} crops the data sets using a shapefile mask.
#'
#' @importFrom raster brick crop extent mask
#' @importFrom R.utils getAbsolutePath
#' @importFrom sf read_sf
#' @param x a character string with the path to the data file. Or a RasterBrick.
#' @param shp_path a character string with the path to the ".shp" file.
#' @param autosave logical FALSE (default). If TRUE data will be automatically stored in the same location of the input file
#' @return A cropped RasterBrick.
#' @export
#' @examples
#' \dontrun{
#' crop_data("gpcp_tp_mm_global_197901_202205_025_monthly.nc", "cze.shp",
#' autosave = TRUE)
#' crop_data("dummie.nc", "cze.shp", autosave = TRUE)
#' }

crop_data <- function(x, shp_path, autosave = FALSE){
  shp_mask <- read_sf(shp_path)
  nc_in <- getAbsolutePath(x)
  checker <- name_check(x)
  if (checker$length == 8) {
    checker$name[4] <- "cropped"
    nc_out <- paste(checker$name, collapse = "_")
    nc_out <- paste0(nc_out, ".nc")
    nc_mid <- sub("(.*/)(.*)", "\\1", nc_in)
    nc_out <- paste0(nc_mid, nc_out)
  } else {
    nc_out <- sub(".nc.*", "", nc_in)
    nc_out <- paste0(nc_out, "_cropped.nc")
  }
  nc_out <- sub(".nc.nc.*", ".nc", nc_out)
  check_out <- exists_check(nc_out)
  if (check_out$exists) stop(check_out$sms)
  if (is.character(x)){
    dummie_brick <- brick(nc_in)
    dummie_extent <- extent(shp_mask) + 1
    dummie_crop <- crop(dummie_brick, dummie_extent, snap = "out")
    dummie_mask <- mask(dummie_crop, shp_mask)
  } else {
    dummie_extent <- extent(shp_mask) + 1
    dummie_crop <- crop(dummie_brick, dummie_extent, snap = "out")
    dummie_mask <- mask(dummie_crop, shp_mask)
  }
  if (autosave){
    saveNC(dummie_mask, nc_out)
    return(invisible())
  } else {
    return(dummie_mask)
  }
}