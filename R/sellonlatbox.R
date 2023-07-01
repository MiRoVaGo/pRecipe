#' Subset a precipitation data product in space
#'
#' The function \code{sellonlatbox} subsets (space) the requested data set and stores it in the same location of the input file.
#'
#' @importFrom methods as
#' @importFrom raster brick crop extent
#' @importFrom R.utils getAbsolutePath
#' @param x a character string with the path to the data file. Or a RasterBrick.
#' @param bbox numeric vector. Bounding box in the form: (xmin, xmax, ymin, ymax).
#' @param autosave logical FALSE (default). If TRUE data will be automatically stored in the same location of the input file
#' @return A subsetted RasterBrick.
#' @export
#' @examples
#' \dontrun{
#' sellonlatbox("gpcp_tp_mm_global_197901_202205_025_monthly.nc",
#' c(12.24, 18.85, 48.56, 51.12), autosave = TRUE)
#' sellonlatbox("dummie.nc", c(12.24, 18.85, 48.56, 51.12), autosave = TRUE)
#' }

sellonlatbox <- function(x, bbox, autosave = FALSE){
  nc_in <- getAbsolutePath(x)
  checker <- name_check(x)
  if (checker$length == 8) {
    checker$name[4] <- "subset"
    nc_out <- paste(checker$name, collapse = "_")
    nc_out <- paste0(nc_out, ".nc")
    nc_mid <- sub("(.*/)(.*)", "\\1", nc_in)
    nc_out <- paste0(nc_mid, nc_out)
  } else {
    nc_out <- sub(".nc.*", "", nc_in)
    nc_out <- paste0(nc_out, "_subset.nc")
  }
  nc_out <- sub(".nc.nc.*", ".nc", nc_out)
  check_out <- exists_check(nc_out)
  if (check_out$exists) stop(check_out$sms)
  if (is.character(x)){
    dummie_brick <- brick(nc_in)
  } else {
    dummie_brick <- x
  }
  lonlatbox <- as(extent(bbox[1], bbox[2], bbox[3], bbox[4]),
                  'SpatialPolygons')
  dummie_subset <- crop(dummie_brick, lonlatbox)
  if (autosave){
    saveNC(dummie_subset, nc_out)
    return(invisible())
  } else {
    return(dummie_subset)
  }
}