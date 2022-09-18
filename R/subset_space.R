#' Subset a precipitation data product in space
#'
#' The function \code{subset_space} subsets (space) the requested data set and stores it in <project_folder>/data/processed. If the input data set is a Raster object the output will be stored in the same location of the input file.
#'
#' @importFrom methods is
#' @param name a Raster object or a character string with the name(s) of the desired data set. Suitable options are:
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
#' @param bbox numeric vector. Bounding box in the form: (xmin, xmax, ymin, ymax).
#' @return No return value, called to subset via cdo
#' @export
#' @examples
#' \dontrun{
#' x <- subset_space("gpcp", c(12.24, 18.85, 48.56, 51.12), tempdir())
#' w <- raste::brick("dummie.nc")
#' z <- subset_space(w, c(12.24, 18.85, 48.56, 51.12), tempdir())
#' }

subset_space <- function(name, bbox, database_path = "./data/database"){
  if (!Reduce("&", is.element(name, c("20cr", "chirps", "cmap", "cmorph", "cpc", "cru_ts", "em_earth", "era20c", "era5", "ghcn", "gldas_clsm", "gldas_noah", "gldas_vic", "gpcc", "gpcp", "gpm_imerg", "mswep", "ncep_doe", "ncep_ncar", "persiann", "precl", "terraclimate", "trmm_3b43", "udel")))){
    if (is(name, "RasterBrick") | is(name, "RasterLayer") | is(name, "RasterStack")){
      nc_in <- name@file@name
      nc_out <- sub(".nc.*", "", nc_in)
      nc_out <- paste0(nc_out, ".subset.nc")
      lonlatbox <- paste(bbox, collapse = ",")
      cdo_str <- paste0("cdo -L -z zip_4 -sellonlatbox,", lonlatbox, " ", nc_in, " ", nc_out)
      system(cdo_str)
      return(invisible())
    } else {
      stop("Error: Data set not available. Select from 20cr, chirps, cmap, cmorph, cpc, cru_ts, em_earth, era20c, era5, ghcn, gldas_clsm, gldas_noah, gldas_vic, gpcc, gpcp, gpm_imerg, mswep, ncep_doe, ncep_ncar, persiann, precl, terraclimate, trmm_3b43, udel")
    }
  }
  if (!grepl("*/data/database", database_path)){
    stop("Error: database_path should point to the location of '<project_folder>/data/database'")
  }
  nc_in <- grep(name, list.files(database_path, full.names = TRUE), value = TRUE)
  nc_end <- substr(nc_in, nchar(nc_in) - 28, nchar(nc_in))
  nc_out <- sub(".tp.*", "", nc_in)
  nc_out <- gsub("database", "processed", nc_out)
  nc_out <- paste0(nc_out, "tp.mm.subset", nc_end)
  lonlatbox <- paste(bbox, collapse = ",")
  cdo_str <- paste0("cdo -L -z zip_4 -sellonlatbox,", lonlatbox, " ", nc_in, " ", nc_out)
  system(cdo_str)
  return(invisible())
}