#' Rescale a precipitation data product in time
#'
#' The function \code{yearstat} aggregates the requested data set from monthly to yearly time steps and stores it in the same location of the input file.
#'
#' @importFrom methods as 
#' @importFrom raster brick setZ subset zApply
#' @importFrom R.utils getAbsolutePath
#' @param x a character string with the path to the data file. Or RasterBrick object
#' @param stat a character string with the desired aggregation function. Suitable options are:
#' \itemize{
#' \item "max"
#' \item "mean"
#' \item "median"
#' \item "min"
#' \item "sum" (default)
#' }
#' @param autosave logical FALSE (default). If TRUE data will be automatically stored in the same location of the input file
#' @return A RasterBrick.
#' @export
#' @examples
#' \dontrun{
#' yearstat("gpcp_tp_mm_global_197901_202205_025_monthly.nc", autosave = TRUE)
#' yearstat("dummie.nc", autosave = TRUE)
#' }

yearstat <- function(x, stat = "sum", autosave = FALSE){
  nc_in <- getAbsolutePath(x)
  checker <- name_check(x)
  if (checker$length == 8) {
    checker$name[8] <- "yearly"
    start_year <- substr(checker$name[5], 1, 4)
    start_month <- substr(checker$name[5], 5, 6)
    end_year <- substr(checker$name[6], 1, 4)
    end_month <- substr(checker$name[6], 5, 6)
    if ((as.numeric(start_month) != 1) & (as.numeric(end_month) != 12)){
      checker$name[5] <- as.numeric(start_year) + 1
      checker$name[6] <- as.numeric(end_year) - 1
    } else if ((as.numeric(start_month) != 1) & (as.numeric(end_month) == 12)){
      checker$name[5] <- as.numeric(start_year) + 1
      checker$name[6] <- end_year
    } else if ((as.numeric(start_month) == 1) & (as.numeric(end_month) != 12)){
      checker$name[5] <- start_year
      checker$name[6] <- as.numeric(end_year) - 1
    } else {
      checker$name[5] <- start_year
      checker$name[6] <- end_year
    }
    nc_out <- paste(checker$name, collapse = "_")
    nc_out <- paste0(nc_out, ".nc")
    nc_mid <- sub("(.*/)(.*)", "\\1", nc_in)
    nc_out <- paste0(nc_mid, nc_out)
  } else {
    nc_out <- sub(".nc.*", "", nc_in)
    nc_out <- paste0(nc_out, "_yearly.nc")
  }
  nc_out <- sub(".nc.nc.*", ".nc", nc_out)
  check_out <- exists_check(nc_out)
  if (check_out$exists) stop(check_out$sms)
  if (is.character(x)){
    dummie_brick <- brick(nc_in)
  } else {
    dummie_brick <- x
  }
  if (checker$length == 8) {
    if ((as.numeric(start_month) != 1) & (as.numeric(end_month) != 12)){
      start_year <- paste0(as.numeric(start_year) + 1, "-01-01")
      end_year <- paste0(as.numeric(end_year) - 1, "-12-01")
    } else if ((as.numeric(start_month) != 1) & (as.numeric(end_month) == 12)){
      start_year <- paste0(as.numeric(start_year) + 1, "-01-01")
      end_year <- paste0(end_year, "-12-01")
    } else if ((as.numeric(start_month) == 1) & (as.numeric(end_month) != 12)){
      start_year <- paste0(start_year, "-01-01")
      end_year <- paste0(as.numeric(end_year) - 1, "-12-01")
    } else {
      start_year <- paste0(start_year, "-01-01")
      end_year <- paste0(end_year, "-12-01")
    }
    dummie_yearly <- zApply(dummie_brick, by = year, fun = match.fun(stat), na.rm = TRUE)
    dummie_yearly <- setZ(dummie_yearly, seq(as.Date(start_year), 
                                             as.Date(end_year), by = "years"))
    range_years <- which(getZ(dummie_yearly) >= start_year & 
                           (getZ(dummie_yearly) <= end_year))
    dummie_yearly <- subset(dummie_yearly, range_years)
    dummie_yearly <- setZ(dummie_yearly, seq(as.Date(start_year), 
                                             as.Date(end_year), by = "years"))
  } else {
    dummie_yearly <- zApply(dummie_brick, by = year, fun = match.fun(stat), na.rm = TRUE)
  }
  if (autosave){
    saveNC(dummie_yearly, nc_out)
    return(invisible())
  } else {
    return(dummie_yearly)
  }
}