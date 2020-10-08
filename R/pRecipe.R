#' Download and import precipitation data from various sources
#'
#' The function \code{capture} downloads the selected data product.
#'
#' @importFrom httr GET
#' @importFrom stringr str_pad
#' @importFrom getPass getPass
#' @param name a character string with the name of the desired data set. Suitable options are:
#' \itemize{
#' \item{"cmap" for CMAP standard version,}
#' \item{"cpc" for CPC-Global,}
#' \item{"cru" for CRU_TS v4.04,}
#' \item{"ghcn" for GHCN-M v2}
#' \item{"gpcc" for GPCC v2018,}
#' \item{"gpcp" for GPCP v2.3,}
#' \item{"gpm" for GPM IMERG Final v06,}
#' \item{"precl" for PRECL,}
#' \item{"trmm" for TRMM 3B43 v7}
#' \item{"udel" for UDEL v501}
#' }
#' @param destination a character string with the path where the downloaded file is saved.
#' @param resolution numeric. Data spatial resolution for GPCC or PRECL. Suitable options are:
#' \itemize{
#' \item{0.25 for 0.25 degree (GPCC only),}
#' \item{0.5 for 0.5 degree,}
#' \item{1 for 1 degree,}
#' \item{2.5 for 2.5 degree.}
#' }
#' @param start_year numeric. Start year should be between 1979-2019 (CPC), 2000-2019 (GPM), or 1998-2019 (TRMM).
#' @param end_year numeric. End year should be between 1979-2019 (CPC), 2000-2019 (GPM), or 1998-2019 (TRMM), and should be greater or equal to start year.
#' @export

capture <- function(name, destination, resolution = NULL, start_year = NULL, end_year = NULL){
  '%!in%' <- function(x, y)!('%in%'(x, y))
  if (name %!in% c("cmap", "cpc", "cru", "ghcn", "gpcc", "gpcp", "gpm", "precl", "trmm", "udel", "persiann", 
                   "persiann_ccs", "ncep1", "ncep2", "20crv3")){
    stop("Error: Data set not supported. Select one of cru, ghcn, gpcc, gpcp, gpm, precl, trmm, udel, 
         cpc_global, persiann, persiann_ccs, gpcp, cmap, ncep1, ncep2, 20crv3")
  }
  switch(name,
         "cmap" = download_cmap(destination),
         "cpc" = download_cpc(destination, start_year, end_year),
         "cru" = download_cru(destination),
         "ghcn" = download_ghcn(destination),
         "gpcc" = download_gpcc(destination, resolution),
         "gpcp" = download_gpcp(destination),
         "gpm" = download_gpm(destination, start_year, end_year),
         "precl" = download_precl(destination, resolution),
         "trmm" = download_trmm(destination, start_year, end_year),
         "udel" = download_udel(destination),

         "persiann" = download_persiann(destination),
         "persiann_ccs" = download_persiann_ccs(destination),
         "ncep1" = download_ncep1(destination),
         "ncep2" = download_ncep2(destination),
         "20crv3" = download_20crv3(destination)
  )
}