#' Generate time series
#'
#' The function \code{make_ts} generates a csv time series and stored in the same location of the input file.
#'
#' @import data.table
#' @importFrom R.utils getAbsolutePath
#' @param data a character string with the path to the data file. Or a RasterBrick
#' @param name a character string with short data name. Suitable options are:
#' \itemize{
#' \item{"20cr" for 20CR v3,}
#' \item{"chirps" for CHIRPS v2.0,}
#' \item{"cmap" for CMAP standard version,}
#' \item{"cmorph" for CMORPH,}
#' \item{"cpc" for CPC-Global,}
#' \item{"cru-ts" for CRU_TS v4.06,}
#' \item{"em-earth" for EM-EARTH,}
#' \item{"era20c" for ERA-20C,}
#' \item{"era5" for ERA5,}
#' \item{"fldas" for FLDAS,}
#' \item{"ghcn" for GHCN-M v2,}
#' \item{"gldas-clsm" for GLDAS CLSM,}
#' \item{"gldas-noah" for GLDAS NOAH,}
#' \item{"gldas-vic" for GLDAS VIC,}
#' \item{"gleam" for GLEAM v3.7a,}
#' \item{"gpcc" for GPCC v2020,}
#' \item{"gpcp" for GPCP v2.3,}
#' \item{"gpm_imerg" for GPM IMERGM Final v06,}
#' \item{"jra55" for JRA-55,}
#' \item{"merra2" for MERRA-2,}
#' \item{"mswep" for MSWEP v2.8,}
#' \item{"ncep-doe" for NCEP/DOE,}
#' \item{"ncep-ncar" for NCEP/NCAR,}
#' \item{"persiann" for PERSIANN-CDR,}
#' \item{"precl" for PREC/L,}
#' \item{"terraclimate" for TerraClimate,}
#' \item{"trmm-3b43" for TRMM 3B43 v7,}
#' \item{"udel" for UDEL v501.}
#' }
#' @param autosave logical FALSE (default). If TRUE data will be automatically stored in the same location of the input file
#' @return A data.table
#' @export
#' @examples
#' \dontrun{
#' make_ts("gpcp_tp_mm_global_197901_202205_025_monthly.nc")
#' make_ts("dummie.nc")
#' }

make_ts <- function(data, name = NULL, autosave = FALSE){
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
  if(is.null(name)){
    dummie_cols <- aux_ts(checker$name[1])
  } else {
    dummie_cols <- aux_ts(name)
  }
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
