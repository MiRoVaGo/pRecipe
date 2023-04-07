#' Download various precipitation data products
#'
#' The function \code{download_data} downloads the selected data product.
#'
#' @param data_name a character string with the name(s) of the desired data set. Suitable options are:
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
#' @param path a character string with the path where the database will be downloaded.
#' @param domain a character string with the desired domain data set. Suitable options are:
#' \itemize{
#' \item{"raw" for default available spatial coverage,}
#' \item{"global" for data sets with global (land and ocean) coverage,}
#' \item{"land" for data sets with land only coverage,}
#' \item{"ocean", for data sets with ocean only coverage.}
#' }
#' @return No return value, called to download the required data sets.
#' @export
#' @examples
#' \donttest{
#' download_data("gldas-vic", tempdir())
#' }

download_data <- function(data_name = "all", path = ".", domain = "raw"){
  dataset_check(data_name)
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  lapply(data_name, function(dataset) switch(dataset,
                                        "all"  = download_all(path, domain),
                                        "20cr" = download_20cr(path, domain),
                                        "chirps" = download_chirps(path, domain),
                                        "cmap" = download_cmap(path, domain),
                                        "cmorph" = download_cmorph(path, domain),
                                        "cpc" = download_cpc(path, domain),
                                        "cru-ts" = download_cru_ts(path, domain),
                                        "em-earth" = download_em_earth(path, domain),
                                        "era20c" = download_era20c(path, domain),
                                        "era5" = download_era5(path, domain),
                                        "ghcn" = download_ghcn(path, domain),
                                        "gldas-clsm" = download_gldas_clsm(path, domain),
                                        "gldas-noah" = download_gldas_noah(path, domain),
                                        "gldas-vic" = download_gldas_vic(path, domain),
                                        "gpcc" = download_gpcc(path, domain),
                                        "gpcp" = download_gpcp(path, domain),
                                        "gpm-imerg" = download_gpm_imerg(path, domain),
                                        "mswep" = download_mswep(path, domain),
                                        "ncep-doe" = download_ncep_doe(path, domain),
                                        "ncep-ncar" = download_ncep_ncar(path, domain),
                                        "persiann" = download_persiann(path, domain),
                                        "precl" = download_precl(path, domain),
                                        "terraclimate" = download_terraclimate(path, domain),
                                        "trmm-3b43" = download_trmm_3b43(path, domain),
                                        "udel" = download_udel(path, domain)
  ))
  return(invisible())
}