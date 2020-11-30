#' Download and import precipitation data from various sources
#'
#' The function \code{download_data} downloads the selected data product.
#'
#' @importFrom stringr str_pad
#' @importFrom getPass getPass
#' @importFrom utils download.file
#' @param name a character string with the name of the desired data set. Suitable options are:
#' \itemize{
#' \item{"20cr" for 20CR v3,}
#' \item{"cmap" for CMAP standard version,}
#' \item{"cpc" for CPC-Global,}
#' \item{"cru" for CRU_TS v4.04,}
#' \item{"ghcn" for GHCN-M v2}
#' \item{"gpcc" for GPCC v2018,}
#' \item{"gpcp" for GPCP v2.3,}
#' \item{"gpm" for GPM IMERG Final v06,}
#' \item{"ncep" for NCEP/NCAR,}
#' \item{"ncep2" for NCEP/DOE,}
#' \item{"precl" for PRECL,}
#' \item{"trmm" for TRMM 3B43 v7,}
#' \item{"udel" for UDEL v501.}
#' }
#' @param destination a character string with the path where the downloaded file is saved.
#' @param resolution numeric. Data spatial resolution for GPCC and PRECL. Suitable options are:
#' \itemize{
#' \item{0.5 for 0.5 degree,}
#' \item{1 for 1 degree,}
#' \item{2.5 for 2.5 degree.}
#' }
#' @param start_year numeric. Start year should be between 1979-2019 (CPC), 2000-2019 (GPM), or 1998-2019 (TRMM).
#' @param end_year numeric. End year should be between 1979-2019 (CPC), 2000-2019 (GPM), or 1998-2019 (TRMM), and should be greater or equal to start year.
#' @export

download_data <- function(name, destination, resolution = NULL, start_year = NULL, end_year = NULL){
  '%!in%' <- function(x, y)!('%in%'(x, y))
  if (name %!in% c("20cr", "cmap", "cpc", "cru", "ghcn", "gpcc", "gpcp", "gpm", "ncep", "ncep2", "precl", "trmm", "udel")){
    stop("Error: Data set not supported. Select one of 20cr, cmap, cpc, cru, ghcn, gpcc, gpcp, gpm, ncep, ncep2, precl, trmm, udel")
  }
  switch(name,
         "20cr" = download_20cr(destination),
         "cmap" = download_cmap(destination),
         "cpc" = download_cpc(destination, start_year, end_year),
         "cru" = download_cru(destination),
         "ghcn" = download_ghcn(destination),
         "gpcc" = download_gpcc(destination, resolution),
         "gpcp" = download_gpcp(destination),
         "gpm" = download_gpm(destination, start_year, end_year),
         "ncep" = download_ncep(destination),
         "ncep2" = download_ncep2(destination),
         "precl" = download_precl(destination, resolution),
         "trmm" = download_trmm(destination, start_year, end_year),
         "udel" = download_udel(destination)
  )
}

#' Read precipitation data from various sources and reformat them into .Rds files
#'
#' The function \code{reformat_data} reformats the selected data product.
#'
#' @import gdalUtils ncdf4 parallel raster rgdal rhdf5
#' @importFrom dplyr %>% slice
#' @importFrom R.utils gunzip
#' @param name a character string with the name of the desired data set. Suitable options are:
#' \itemize{
#' \item{"20cr" for 20CR v3,}
#' \item{"cmap" for CMAP standard version,}
#' \item{"cpc" for CPC-Global,}
#' \item{"cru" for CRU_TS v4.04,}
#' \item{"ghcn" for GHCN-M v2}
#' \item{"gpcc" for GPCC v2018,}
#' \item{"gpcp" for GPCP v2.3,}
#' \item{"gpm" for GPM IMERG Final v06,}
#' \item{"ncep" for NCEP/NCAR,}
#' \item{"ncep2" for NCEP/DOE,}
#' \item{"precl" for PRECL,}
#' \item{"trmm" for TRMM 3B43 v7,}
#' \item{"udel" for UDEL v501.}
#' }
#' @param folder_path a character string with the folder path where the data set is located.
#' @param save logical. If TRUE (default) an .Rds file will be saved in the same location.
#' @param preserve logical. If TRUE (default) the original file will be preserved.
#' @export

reformat_data <- function(name, folder_path, save = TRUE, preserve = TRUE){
  '%!in%' <- function(x, y)!('%in%'(x, y))
  if (name %!in% c("20cr", "cmap", "cpc", "cru", "ghcn", "gpcc", "gpcp", "gpm", "ncep", "ncep2", "precl", "trmm", "udel")){
    stop("Error: Data set not supported. Select one of 20cr, cmap, cpc, cru, ghcn, gpcc, gpcp, gpm, ncep, ncep2, precl, trmm, udel")
  }
  switch(name,
         "20cr" = {precip_20cr <<- reformat_20cr(folder_path, save, preserve)},
         "cmap" = {precip_cmap <<- reformat_cmap(folder_path, save, preserve)},
         "cpc" = {precip_cpc <<- reformat_cpc(folder_path, save, preserve)},
         "cru" = {precip_cru <<- reformat_cru(folder_path, save, preserve)},
         "ghcn" = {precip_ghcn <<- reformat_ghcn(folder_path, save, preserve)},
         "gpcc" = {precip_gpcc <<- reformat_gpcc(folder_path, save, preserve)},
         "gpcp" = {precip_gpcp <<- reformat_gpcp(folder_path, save, preserve)},
         "gpm" = {precip_gpm <<- reformat_gpm(folder_path, save, preserve)},
         "ncep" = {precip_ncep <<- reformat_ncep(folder_path, save, preserve)},
         "ncep2" = {precip_ncep2 <<- reformat_ncep2(folder_path, save, preserve)},
         "precl" = {precip_precl <<- reformat_precl(folder_path, save, preserve)},
         "trmm" = {precip_trmm <<- reformat_trmm(folder_path, save, preserve)},
         "udel" = {precip_udel <<- reformat_udel(folder_path, save, preserve)}
  )
}

#' Transforms the data from precipitation rate into total precipitation.
#'
#' The function \code{prate_to_psum} accumulates precipitation of the selected data product.
#'
#' @import gdalUtils ncdf4 parallel raster rgdal rhdf5
#' @importFrom dplyr %>% slice
#' @importFrom lubridate days_in_month
#' @param name a character string with the name of the desired data set. Suitable options are:
#' \itemize{
#' \item{"cmap" for CMAP standard version,}
#' \item{"gpcp" for GPCP v2.3,}
#' \item{"gpm" for GPM IMERG Final v06,}
#' \item{"ncep" for NCEP/NCAR,}
#' \item{"ncep2" for NCEP/DOE,}
#' \item{"precl" for PRECL,}
#' \item{"trmm" for TRMM 3B43 v7,}
#' \item{"udel" for UDEL v501.}
#' }
#' @param file a character string with the folder path where the data set is located.
#' @param overwrite logical. If TRUE (default) the original file with precipitation data will be replaced with a file with total precipitation.
#' @note For Udel data only the units are transformed to mm because it is already in terms of total precipitation
#' @export

prate_to_psum <- function(name, file, overwrite = TRUE){
  '%!in%' <- function(x, y)!('%in%'(x, y))
  if (name %!in% c("cmap", "gpcp", "gpm", "ncep", "ncep2", "precl", "trmm", "udel")){
    stop("Error: Data set not supported. Select one of 20cr, cmap, cpc, cru, ghcn, gpcc, gpcp, gpm, ncep, ncep2, precl, trmm, udel")
  }
  switch(name,
         "cmap" = {precip_cmap <<- prate_to_psum_cmap(file, overwrite)},
         "gpcp" = {precip_gpcp <<- prate_to_psum_gpcp(file, overwrite)},
         "gpm" = {precip_gpm <<- prate_to_psum_gpm(file, overwrite)},
         "ncep" = {precip_ncep <<- prate_to_psum_ncep(file, overwrite)},
         "ncep2" = {precip_ncep2 <<- prate_to_psum_ncep2(file, overwrite)},
         "precl" = {precip_precl <<- prate_to_psum_precl(file, overwrite)},
         "trmm" = {precip_trmm <<- prate_to_psum_trmm(file, overwrite)},
         "udel" = {precip_udel <<- prate_to_psum_udel(file, overwrite)}
  )
}