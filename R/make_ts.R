#' Generate time series
#'
#' The function \code{make_ts} generates a csv time series and stored in the same location of the input file.
#'
#' @import data.table
#' @importFrom R.utils getAbsolutePath
#' @param data_file a character string with the path to the data file.
#' @return No return value, called to generate and store csv time series.
#' @export
#' @examples
#' \dontrun{
#' make_ts("gpcp_tp_mm_global_197901_202205_025_monthly.nc")
#' make_ts("dummie.nc")
#' }

make_ts <- function(data_file){
  nc_in <- getAbsolutePath(data_file)
  checker <- name_check(data_file)
  if (checker$length == 8) {
    nc_out <- paste(checker$name, collapse = "_")
    nc_out <- sub(".nc.*", "", nc_out)
    nc_out <- paste0(nc_out, "_ts.csv")
    nc_mid <- sub("(.*/)(.*)", "\\1", nc_in)
    nc_out <- paste0(nc_mid, nc_out)
  } else {
    warning("This is not pRecipe data")
    nc_out <- sub(".nc.*", "", nc_in)
    nc_out <- paste0(nc_out, "_ts.csv")
  }
  nc_out <- sub(".nc.nc.*", ".nc", nc_out)
  check_out <- exists_check(nc_out)
  if (check_out$exists) stop(check_out$sms)
  if (Sys.info()['sysname'] == "Windows") {
    csv_table <- weighted_average(nc_in)
    fwrite(csv_table, nc_out)
  } else {
    cdo_str <- paste0("cdo -L -z zip_4 -outputtab,nohead,date,value -fldmean ", nc_in, " >> ", nc_out)
    system(cdo_str)
    cdo_csv(nc_out)
  }
  return(invisible())
}
