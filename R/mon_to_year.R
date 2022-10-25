#' Rescale a precipitation data product in time
#'
#' The function \code{mon_to_year} aggregates the requested data set from monthly to yearly time steps and stores it in the same location of the input file.
#'
#' @importFrom methods as 
#' @importFrom raster brick setZ subset zApply
#' @importFrom R.utils getAbsolutePath
#' @param data_file a character string with the path to the data file.
#' @return No return value, called to aggregate and store store the new data file.
#' @export
#' @examples
#' \dontrun{
#' mon_to_year("gpcp_tp_mm_global_197901_202205_025_monthly.nc")
#' mon_to_year("dummie.nc")
#' }

mon_to_year <- function(data_file){
  nc_in <- getAbsolutePath(data_file)
  checker <- name_check(data_file)
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
    warning("This is not pRecipe data")
    nc_out <- sub(".nc.*", "", nc_in)
    nc_out <- paste0(nc_out, "_yearly.nc")
  }
  nc_out <- sub(".nc.nc.*", ".nc", nc_out)
  check_out <- exists_check(nc_out)
  if (check_out$exists) stop(check_out$sms)
  if (Sys.info()['sysname'] == "Windows") {
    dummie_brick <- brick(nc_in)
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
      dummie_yearly <- zApply(dummie_brick, by = year, fun = sum, na.rm = TRUE)
      dummie_yearly <- setZ(dummie_yearly, seq(as.Date(start_year), 
                                               as.Date(end_year), by = "years"))
      range_years <- which(getZ(dummie_yearly) >= start_year & 
                             (getZ(dummie_yearly) <= end_year))
      dummie_yearly <- subset(dummie_yearly, range_years)
      dummie_yearly <- setZ(dummie_yearly, seq(as.Date(start_year), 
                                               as.Date(end_year), by = "years"))
    } else {
      dummie_yearly <- zApply(dummie_brick, by = year, fun = sum, na.rm = TRUE)
    }
    save_nc(dummie_yearly, nc_out)
  } else {
    if ((as.numeric(start_month) != 1) & (as.numeric(end_month) != 12)){
      cdo_str <- paste0("cdo --no_warnings -L -z zip_4 -setmon,1 -setday,1 -yearsum -delete,year=", start_year, ",", end_year, " ", nc_in, " ", nc_out)
    } else if ((as.numeric(start_month) != 1) & (as.numeric(end_month) == 12)){
      cdo_str <- paste0("cdo --no_warnings -L -z zip_4 -setmon,1 -setday,1 -yearsum -delete,year=", start_year, " ", nc_in, " ", nc_out)
    } else if ((as.numeric(start_month) == 1) & (as.numeric(end_month) != 12)){
      cdo_str <- paste0("cdo --no_warnings -L -z zip_4 -setmon,1 -setday,1 -yearsum -delete,year=", end_year, " ", nc_in, " ", nc_out)
    } else {
      cdo_str <- paste0("cdo --no_warnings -L -z zip_4 -setmon,1 -setday,1 -yearsum ", nc_in, " ", nc_out)
    }
    system(cdo_str)
  }
  return(invisible())
}