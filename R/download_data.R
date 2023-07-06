#' Download various precipitation data products
#'
#' The function \code{download_data} downloads the selected data product.
#'
#' @param dataset a character string with the name(s) of the desired data set. Suitable options are:
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
#' \item{"fldas" for FLDAS,}
#' \item{"ghcn" for GHCN-M v2,}
#' \item{"gldas-clsm" for GLDAS CLSM,}
#' \item{"gldas-noah" for GLDAS NOAH,}
#' \item{"gldas-vic" for GLDAS VIC,}
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
#' @param path a character string with the path where the database will be downloaded.
#' @param domain a character string with the desired domain data set. Suitable options are:
#' \itemize{
#' \item{"raw" for default available spatial coverage,}
#' \item{"global" for data sets with global (land and ocean) coverage,}
#' \item{"land" for data sets with land only coverage,}
#' \item{"ocean", for data sets with ocean only coverage.}
#' }
#' @param timestep a character string with the desired time resolution. Suitable options are:
#' \itemize{
#' \item{"monthly",}
#' \item{"yearly".}
#' }
#' @return No return value, called to download the required data sets.
#' @export
#' @examples
#' \donttest{
#' download_data("gldas-vic", tempdir(), timestep = "yearly")
#' }

download_data <- function(dataset = "all", path = ".", domain = "raw", timestep = "monthly"){
  dataset_check(dataset)
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  lapply(dataset, function(dataset) switch(dataset,
                                        "all"  = download_all(path, domain, timestep),
                                        "20cr" = download_20cr(path, domain, timestep),
                                        "chirps" = download_chirps(path, domain, timestep),
                                        "cmap" = download_cmap(path, domain, timestep),
                                        "cmorph" = download_cmorph(path, domain, timestep),
                                        "cpc" = download_cpc(path, domain, timestep),
                                        "cru-ts" = download_cru_ts(path, domain, timestep),
                                        "em-earth" = download_em_earth(path, domain, timestep),
                                        "era20c" = download_era20c(path, domain, timestep),
                                        "era5" = download_era5(path, domain, timestep),
                                        "fldas" = download_fldas(path, domain, timestep),
                                        "ghcn" = download_ghcn(path, domain, timestep),
                                        "gldas-clsm" = download_gldas_clsm(path, domain, timestep),
                                        "gldas-noah" = download_gldas_noah(path, domain, timestep),
                                        "gldas-vic" = download_gldas_vic(path, domain, timestep),
                                        "gpcc" = download_gpcc(path, domain, timestep),
                                        "gpcp" = download_gpcp(path, domain, timestep),
                                        "gpm-imerg" = download_gpm_imerg(path, domain, timestep),
                                        "jra55" = download_jra55(path, domain, timestep),
                                        "merra2" = download_merra2(path, domain, timestep),
                                        "mswep" = download_mswep(path, domain, timestep),
                                        "ncep-doe" = download_ncep_doe(path, domain, timestep),
                                        "ncep-ncar" = download_ncep_ncar(path, domain, timestep),
                                        "persiann" = download_persiann(path, domain, timestep),
                                        "precl" = download_precl(path, domain, timestep),
                                        "terraclimate" = download_terraclimate(path, domain, timestep),
                                        "trmm-3b43" = download_trmm_3b43(path, domain, timestep),
                                        "udel" = download_udel(path, domain, timestep)
  ))
  return(invisible())
}
