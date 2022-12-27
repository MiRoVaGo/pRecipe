#' Subset a precipitation data product in space
#'
#' The function \code{rescale_data} aggregates the requested data sets into desired resolution and stores it in the same location of the input file.
#'
#' @importFrom raster raster res
#' @importFrom R.utils getAbsolutePath
#' @param data_file a character string with the path to the data file.
#' @param new_res numeric. Target resolution must be a multiple of 0.25 (e.g., 0.5, 1, 2.5).
#' @return No return value, called to aggregate and store the data file.
#' @export
#' @examples
#' \dontrun{
#' x <- rescale_data("gpcp_tp_mm_global_197901_202205_025_monthly.nc", 1)
#' z <- rescale_data("dummie.nc", 1)
#' }

rescale_data <- function(data_file, new_res){
  if (new_res%%0.25 != 0) {
    stop("Error: New resolution must be a multiple of 0.25")
  }
  nc_in <- getAbsolutePath(data_file)
  checker <- name_check(data_file)
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
    warning("This is not pRecipe data")
    nc_out <- sub(".nc.*", "", nc_in)
    nc_out <- paste0(nc_out, "_aggregated.nc")
  }
  nc_out <- sub(".nc.nc.*", ".nc", nc_out)
  check_out <- exists_check(nc_out)
  if (check_out$exists) stop(check_out$sms)
  dummie_aggregated <- aggregate_brick(nc_in, new_res)
  save_nc(dummie_aggregated, nc_out)
  return(invisible())
}