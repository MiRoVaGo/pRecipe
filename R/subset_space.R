#' Subset a precipitation data product in space
#'
#' The function \code{subset_space} subsets (space) the requested data set and stores it in the same location of the input file.
#'
#' @importFrom methods as
#' @importFrom raster brick crop extent
#' @importFrom R.utils getAbsolutePath
#' @param data_file a character string with the path to the data file.
#' @param bbox numeric vector. Bounding box in the form: (xmin, xmax, ymin, ymax).
#' @return No return value, called to subset and store store the new data file.
#' @export
#' @examples
#' \dontrun{
#' subset_space("gpcp_tp_mm_global_197901_202205_025_monthly.nc",
#' c(12.24, 18.85, 48.56, 51.12))
#' subset_space("dummie.nc", c(12.24, 18.85, 48.56, 51.12))
#' }

subset_space <- function(data_file, bbox){
  nc_in <- getAbsolutePath(data_file)
  checker <- name_check(data_file)
  if (checker$length == 8) {
    checker$name[4] <- "subset"
    nc_out <- paste(checker$name, collapse = "_")
    nc_out <- paste0(nc_out, ".nc")
    nc_mid <- sub("(.*/)(.*)", "\\1", nc_in)
    nc_out <- paste0(nc_mid, nc_out)
  } else {
    warning("This is not pRecipe data")
    nc_out <- sub(".nc.*", "", nc_in)
    nc_out <- paste0(nc_out, "_subset.nc")
  }
  nc_out <- sub(".nc.nc.*", ".nc", nc_out)
  check_out <- exists_check(nc_out)
  if (check_out$exists) stop(check_out$sms)
  dummie_brick <- brick(nc_in)
  lonlatbox <- as(extent(bbox[1], bbox[2], bbox[3], bbox[4]),
                  'SpatialPolygons')
  dummie_subset <- crop(dummie_brick, lonlatbox)
  save_nc(dummie_subset, nc_out)
  return(invisible())
}