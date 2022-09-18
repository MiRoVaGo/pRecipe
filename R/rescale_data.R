#' Subset a precipitation data product in space
#'
#' The function \code{rescale_data} aggregates the requested data sets into desired resolution and stores it in <project_folder>/data/processed. If the input data set is a Raster object the output will be stored in the same location of the input file.
#'
#' @importFrom methods is
#' @importFrom raster res
#' @param name a character string with the name(s) of the desired data set. Suitable options are:
#' \itemize{
#' \item{"all" for all of the below listed data sets (default),}
#' \item{"20cr" for 20CR v3,}
#' \item{"chirps" for CHIRPS v2.0,}
#' \item{"cmap" for CMAP standard version,}
#' \item{"cmorph" for CMORPH,}
#' \item{"cpc" for CPC-Global,}
#' \item{"cru_ts" for CRU_TS v4.06,}
#' \item{"em_earth" for EM-EARTH,}
#' \item{"era20c" for ERA-20C,}
#' \item{"era5" for ERA5,}
#' \item{"ghcn" for GHCN-M v2,}
#' \item{"gldas_clsm" for GLDAS CLSM,}
#' \item{"gldas_noah" for GLDAS NOAH,}
#' \item{"gldas_vic" for GLDAS VIC,}
#' \item{"gpcc" for GPCC v2020,}
#' \item{"gpcp" for GPCP v2.3,}
#' \item{"gpm_imerg" for GPM IMERGM Final v06,}
#' \item{"mswep" for MSWEP v2.8,}
#' \item{"ncep_doe" for NCEP/DOE,}
#' \item{"ncep_ncar" for NCEP/NCAR,}
#' \item{"persiann" for PERSIANN-CDR,}
#' \item{"precl" for PREC/L,}
#' \item{"terraclimate" for TerraClimate,}
#' \item{"trmm_3b43" for TRMM 3B43 v7,}
#' \item{"udel" for UDEL v501.}
#' }
#' @param database_path a character string with the path where the "database" folder is located.
#' @param new_res numeric. Target resolution must be a multiple of 0.25 (e.g., 0.5, 1, 2.5).
#' @return No return value, called to subset via cdo
#' @export
#' @examples
#' \dontrun{
#' x <- rescale_data("gpcp", 1, tempdir())
#' w <- raster::brick("dummie.nc")
#' z <- rescale_data(w, 1, tempdir())
#' }

rescale_data <- function(name, new_res, database_path = "./data/database"){
  if (!Reduce("&", is.element(name, c("20cr", "chirps", "cmap", "cmorph", "cpc", "cru_ts", "em_earth", "era20c", "era5", "ghcn", "gldas_clsm", "gldas_noah", "gldas_vic", "gpcc", "gpcp", "gpm_imerg", "mswep", "ncep_doe", "ncep_ncar", "persiann", "precl", "terraclimate", "trmm_3b43", "udel")))){
    if (is(name, "RasterBrick") | is(name, "RasterLayer") | is(name, "RasterStack")){
      if (new_res%%0.25 != 0) {
        stop("Error: target resolution must be a multiple of 0.25")
      }
      nc_in <- name@file@name
      nc_out <- sub(".nc.*", "", nc_in)
      nc_out <- paste0(nc_out, ".aggregated.nc")
      new_res <- new_res/res(name)[1]
      new_res <- paste(new_res, new_res, sep = ",")
      cdo_str <- paste0("cdo -L -z zip_4 -gridboxmean,", new_res, " ", nc_in, " ", nc_out)
      system(cdo_str)
      return(invisible())
    } else {
      stop("Error: Data set not available. Select from 20cr, chirps, cmap, cmorph, cpc, cru_ts, em_earth, era20c, era5, ghcn, gldas_clsm, gldas_noah, gldas_vic, gpcc, gpcp, gpm_imerg, mswep, ncep_doe, ncep_ncar, persiann, precl, terraclimate, trmm_3b43, udel")
    }
  }
  if (new_res%%0.25 != 0) {
    stop("Error: target resolution must be a multiple of 0.25")
  }
  if (!grepl("*/data/database", database_path)){
    stop("Error: database_path should point to the location of '<project_folder>/data/database'")
  }
  nc_in <- grep(name, list.files(database_path, full.names = TRUE), value = TRUE)
  nc_out <- sub(".025.*", "", nc_in)
  nc_out <- gsub("database", "processed", nc_out)
  if (new_res < 1) {
    nc_out <- paste(nc_out, sub("\\.", "", new_res), ".monthly.nc", sep = ".")
  } else {
    nc_out <- paste(nc_out, sub("\\.", "dot", new_res), ".monthly.nc", sep = ".")
  }
  new_res <- new_res/0.25
  new_res <- paste(new_res, new_res, sep = ",")
  cdo_str <- paste0("cdo -L -z zip_4 -gridboxmean,", new_res, " ", nc_in, " ", nc_out)
  system(cdo_str)
  return(invisible())
}