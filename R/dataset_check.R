#' Data set name checker
#'
#' Function to check if the data set is available
#'
#' @importFrom methods is
#' @param data_name a character string.
#' @return No return value, called to download the data set.
#' @keywords internal

dataset_check <- function(data_name){
  if (!Reduce("&", is.element(data_name, c("all", "20cr", "chirps", "cmap",
                                           "cmorph", "cpc", "cru-ts",
                                           "em-earth", "era20c", "era5",
                                           "era5-land", "fldas", "ghcn",
                                           "gldas-clsm", "gldas-noah",
                                           "gldas-vic", "gpcc", "gpcp",
                                           "gpm-imerg", "gsmap", "jra55",
                                           "mswep", "merra2", "ncep-doe",
                                           "ncep-ncar", "persiann", "precl",
                                           "terraclimate")))){
    stop("Error: Data set not available.
    Select from 20cr, chirps, cmap, cmorph, cpc, cru-ts, em-earth, era20c,
    era5, era5-land, fldas, ghcn, gldas-clsm, gldas-noah, gldas-vic, gpcc, gpcp,
    gpm-imerg, gsmap, jra55, merra2, mswep, ncep-doe, ncep-ncar, persiann,
    precl, terraclimate")
  }
}