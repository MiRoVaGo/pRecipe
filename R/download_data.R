#' Download various precipitation data products
#'
#' The function \code{download_data} downloads the selected data product.
#'
#' @importFrom methods is
#' @param project_folder a character string with the path where pRecipe will be hosted. Inside it the required subfolders will be created.
#' @param name a character string with the name(s) of the desired data set. Suitable options are:
#' \itemize{
#' \item{"all" for all of the below listed data sets (default),}
#' \item{"20cr" for 20CR v3,}
#' \item{"chirps" for CHIRPS v2.0,}
#' \item{"cmap" for CMAP standard version,}
#' \item{"cmorph" for CMORPH,}
#' \item{"cpc" for CPC-Global,}
#' \item{"cru-ts" for CRU_TS v4.06,}
#' \item{"em-earth" for EM-EARTH,}
#' \item{"era20c" for ERA-20C,}
#' \item{"era5" for ERA5,}
#' \item{"ghcn" for GHCN-M v2,}
#' \item{"gldas-clsm" for GLDAS CLSM,}
#' \item{"gldas-noah" for GLDAS NOAH,}
#' \item{"gldas-vic" for GLDAS VIC,}
#' \item{"gpcc" for GPCC v2020,}
#' \item{"gpcp" for GPCP v2.3,}
#' \item{"gpm_imerg" for GPM IMERGM Final v06,}
#' \item{"mswep" for MSWEP v2.8,}
#' \item{"ncep-doe" for NCEP/DOE,}
#' \item{"ncep-ncar" for NCEP/NCAR,}
#' \item{"persiann" for PERSIANN-CDR,}
#' \item{"precl" for PREC/L,}
#' \item{"terraclimate" for TerraClimate,}
#' \item{"trmm-3b43" for TRMM 3B43 v7,}
#' \item{"udel" for UDEL v501.}
#' }
#' @return No return value, called to download the required data sets.
#' @export
#' @examples
#' \donttest{
#' download_data("gldas-vic", tempdir())
#' }

download_data <- function(name = "all", project_folder = "."){
  if (!Reduce("&", is.element(name, c("all", "20cr", "chirps", "cmap", "cmorph", "cpc", "cru-ts", "em-earth", "era20c", "era5", "ghcn", "gldas-clsm", "gldas-noah", "gldas-vic", "gpcc", "gpcp", "gpm-imerg", "mswep", "ncep-doe", "ncep-ncar", "persiann", "precl", "terraclimate", "trmm-3b43", "udel")))){
    stop("Error: Data set not available. Select from 20cr, chirps, cmap, cmorph, cpc, cru-ts, em-earth, era20c, era5, ghcn, gldas-clsm, gldas-noah, gldas-vic, gpcc, gpcp, gpm-imerg, mswep, ncep-doe, ncep-ncar, persiann, precl, terraclimate, trmm-3b43, udel")
  }
  create_folders(project_folder)
  destination <- paste0(project_folder,"/data/database/")
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  lapply(name, function(dataset) switch(dataset,
                                        "all"  = download_all(destination),
                                        "20cr" = download_20cr(destination),
                                        "chirps" = download_chirps(destination),
                                        "cmap" = download_cmap(destination),
                                        "cmorph" = download_cmorph(destination),
                                        "cpc" = download_cpc(destination),
                                        "cru-ts" = download_cru_ts(destination),
                                        "em-earth" = download_em_earth(destination),
                                        "era20c" = download_era20c(destination),
                                        "era5" = download_era5(destination),
                                        "ghcn" = download_ghcn(destination),
                                        "gldas-clsm" = download_gldas_clsm(destination),
                                        "gldas-noah" = download_gldas_noah(destination),
                                        "gldas-vic" = download_gldas_vic(destination),
                                        "gpcc" = download_gpcc(destination),
                                        "gpcp" = download_gpcp(destination),
                                        "gpm-imerg" = download_gpm_imerg(destination),
                                        "mswep" = download_mswep(destination),
                                        "ncep-doe" = download_ncep_doe(destination),
                                        "ncep-ncar" = download_ncep_ncar(destination),
                                        "persiann" = download_persiann(destination),
                                        "precl" = download_precl(destination),
                                        "terraclimate" = download_terraclimate(destination),
                                        "trmm-3b43" = download_trmm_3b43(destination),
                                        "udel" = download_udel(destination)
  ))
  return(invisible())
}