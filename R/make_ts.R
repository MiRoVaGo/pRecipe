#' Generate time series
#'
#' The function \code{make_ts} generates a csv time series and stored in the same location of the input file.
#'
#' @import data.table
#' @importFrom R.utils getAbsolutePath
#' @param data a character string with the path to the data file. Or a RasterBrick
#' @param autosave logical FALSE (default). If TRUE data will be automatically stored in the same location of the input file
#' @return A data.table
#' @export
#' @examples
#' \dontrun{
#' make_ts("gpcp_tp_mm_global_197901_202205_025_monthly.nc")
#' make_ts("dummie.nc")
#' }

make_ts <- function(data, autosave = FALSE){
  nc_in <- getAbsolutePath(data)
  checker <- name_check(data)
  if (checker$length == 8) {
    nc_out <- paste(checker$name, collapse = "_")
    nc_out <- sub(".nc$", "", nc_out)
    nc_out <- paste0(nc_out, "_ts.csv")
    nc_mid <- sub("(.*/)(.*)", "\\1", nc_in)
    nc_out <- paste0(nc_mid, nc_out)
  } else {
    warning("This is not pRecipe data")
    nc_out <- sub(".nc$", "", nc_in)
    nc_out <- paste0(nc_out, "_ts.csv")
  }
  nc_out <- sub(".nc.nc.*", ".nc", nc_out)
  check_out <- exists_check(nc_out)
  if (check_out$exists) stop(check_out$sms)
  dummie_cols <- aux_ts(checker$name[1])
  if (is.character(data)){
    csv_table <- weighted_average(nc_in)
  } else {
    csv_table <- weighted_average(data)
  }
  csv_table$name <- dummie_cols[1]
  csv_table$type <- dummie_cols[2]
  if (autosave){
    fwrite(csv_table, nc_out)
    return(invisible())
  } else {
    return(csv_table)
  }
}
