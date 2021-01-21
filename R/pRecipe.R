#' Download and import precipitation data from various sources
#'
#' The function \code{download_data} downloads the selected data product.
#'
#' @import data.table gdalUtils ggplot2 ncdf4 parallel rgdal
#' @importFrom dplyr %>%
#' @importFrom getPass getPass
#' @importFrom lubridate days_in_month
#' @importFrom raster aggregate as.data.frame as.list brick disaggregate extend extent flip metadata ncell raster rasterFromXYZ res resample setValues setZ t zApply
#' @importFrom R.utils gunzip
#' @importFrom stringr str_pad
#' @importFrom utils download.file
#' @importFrom zoo as.yearmon as.Date.yearmon
#' @param destination a character string with the path where the downloaded files will be saved.
#' @param name a character string with the name(s) of the desired data set. Suitable options are:
#' \itemize{
#' \item{"all" for all of the below listed data sets (default),}
#' \item{"20cr" for 20CR v3,}
#' \item{"cmap" for CMAP standard version,}
#' \item{"cpc" for CPC-Global,}
#' \item{"cru_ts" for CRU_TS v4.04,}
#' \item{"ghcn" for GHCN-M v2}
#' \item{"gpcc" for GPCC v2018,}
#' \item{"gpcp" for GPCP v2.3,}
#' \item{"gpm_imergm" for GPM IMERGM Final v06,}
#' \item{"ncep_ncar" for NCEP/NCAR,}
#' \item{"ncep_doe" for NCEP/DOE,}
#' \item{"precl" for PRECL,}
#' \item{"trmm_3b43" for TRMM 3B43 v7,}
#' \item{"udel" for UDEL v501.}
#' }
#' @param reformat logical. If TRUE the downloaded datasets are reformatted into data.table and stored in .Rds files
#' @export

download_data <- function(destination, name = "all", reformat = FALSE){
  if (!Reduce("&", is.element(name, c("20cr", "all", "cmap", "cpc", "cru_ts", "ghcn", "gpcc", "gpcp", "gpm_imergm", "ncep_ncar", "ncep_doe", "precl", "trmm_3b43", "udel")))){
    stop("Error: Data set not supported. Select from 20cr, cmap, cpc, cru_ts, ghcn, gpcc, gpcp, gpm_imergm, ncep_ncar, ncep_doe, precl, trmm_3b43, udel")
  }
  create_folders(destination)
  destination <- paste0(destination,"/data/raw")
  lapply(name, function(dataset) switch(dataset,
         "20cr" = download_20cr(destination),
         "all"  = download_all(destination),
         "cmap" = download_cmap(destination),
         "cpc" = download_cpc(destination),
         "cru_ts" = download_cru_ts(destination),
         "ghcn" = download_ghcn(destination),
         "gpcc" = download_gpcc(destination),
         "gpcp" = download_gpcp(destination),
         "gpm_imergm" = download_gpm_imergm(destination),
         "ncep_ncar" = download_ncep_ncar(destination),
         "ncep_doe" = download_ncep_doe(destination),
         "precl" = download_precl(destination),
         "trmm_3b43" = download_trmm_3b43(destination),
         "udel" = download_udel(destination)
  ))
  if (refromat == TRUE) reformat_data(destination, name)
  return(invisible())
}

#' Reformat the downloaded data sets into .Rds files
#'
#' The function \code{reformat_data} reformats the data sets into monthly total precipitation data.tables at 0.5 degree resolution. 
#'
#' @param name a character string with the name of the desired data set. Suitable options are:
#' \itemize{
#' \item{"all" for all of the below listed data sets (default),}
#' \item{"20cr" for 20CR v3,}
#' \item{"cmap" for CMAP standard version,}
#' \item{"cpc" for CPC-Global,}
#' \item{"cru_ts" for CRU_TS v4.04,}
#' \item{"ghcn" for GHCN-M v2}
#' \item{"gpcc" for GPCC v2018,}
#' \item{"gpcp" for GPCP v2.3,}
#' \item{"gpm_imergm" for GPM IMERGM Final v06,}
#' \item{"ncep" for NCEP/NCAR,}
#' \item{"ncep2" for NCEP/DOE,}
#' \item{"precl" for PRECL,}
#' \item{"trmm_3b43" for TRMM 3B43 v7,}
#' \item{"udel" for UDEL v501.}
#' }
#' @param folder_path a character string with the path where the "raw" folder is located.
#' @export

reformat_data <- function(folder_path, name = "all"){
  if (!Reduce("&", is.element(name, c("20cr", "cmap", "cpc", "cru_ts", "ghcn", "gpcc", "gpcp", "gpm_imergm", "ncep_ncar", "ncep_doe", "precl", "trmm_3b43", "udel")))){
    stop("Error: Data set not supported. Select from 20cr, cmap, cpc, cru_ts, ghcn, gpcc, gpcp, gpm_imergm, ncep_ncar, ncep_doe, precl, trmm_3b43, udel")
  }
  lapply(name, function(dataset) switch(dataset,
         "20cr" = reformat_20cr(folder_path),
         "all" = reformat_all(folder_path),
         "cmap" = reformat_cmap(folder_path),
         "cpc" = reformat_cpc(folder_path),
         "cru_ts" = reformat_cru_ts(folder_path),
         "ghcn" = reformat_ghcn(folder_path),
         "gpcc" = reformat_gpcc(folder_path),
         "gpcp" = reformat_gpcp(folder_path),
         "gpm_imergm" = reformat_gpm_imergm(folder_path),
         "ncep_ncar" = reformat_ncep_ncar(folder_path),
         "ncep_doe" = reformat_ncep_doe(folder_path),
         "precl" = reformat_precl(folder_path),
         "trmm_3b43" = reformat_trmm_3b43(folder_path),
         "udel" = reformat_udel(folder_path)
  ))
  return(invisible())
}

#' Read precipitation data.table from database
#'
#' The function \code{import_data} imports the requested data sets.
#'
#' @param name a character string with the name of the desired data set. Suitable options are:
#' \itemize{
#' \item{"all" for all of the below listed data sets,}
#' \item{"20cr" for 20CR v3,}
#' \item{"cmap" for CMAP standard version,}
#' \item{"cpc" for CPC-Global,}
#' \item{"cru_ts" for CRU_TS v4.04,}
#' \item{"ghcn" for GHCN-M v2}
#' \item{"gpcc" for GPCC v2018,}
#' \item{"gpcp" for GPCP v2.3,}
#' \item{"gpm_imergm" for GPM IMERGM Final v06,}
#' \item{"ncep" for NCEP/NCAR,}
#' \item{"ncep2" for NCEP/DOE,}
#' \item{"precl" for PRECL,}
#' \item{"trmm_3b43" for TRMM 3B43 v7,}
#' \item{"udel" for UDEL v501.}
#' }
#' @param folder_path a character string with the path where the "database" folder is located.
#' @return a data.table with the requested precipitation data sets.
#' @export

import_data <- function(folder_path, name){
  if (!Reduce("&", is.element(name, c("20cr", "all", "cmap", "cpc", "cru_ts", "ghcn", "gpcc", "gpcp", "gpm_imergm", "ncep_ncar", "ncep_doe", "precl", "trmm_3b43", "udel")))){
    stop("Error: Data set not supported. Select from 20cr, cmap, cpc, cru_ts, ghcn, gpcc, gpcp, gpm_imergm, ncep_ncar, ncep_doe, precl, trmm_3b43, udel")
  }
  if (name == "all"){
    name <- list.files(folder_path, full.names = TRUE)
  } else {
    
    name <- grep(paste(name, collapse = "|"), list.files(folder_path, full.names = TRUE), value = TRUE)
  }
  precip <- lapply(name, readRDS) %>% rbindlist()
  return(precip)
}

#' Subset precipitation data sets
#'
#' The function \code{subset_data} subsets the imported data sets.
#'
#' @param x a pRecipe data.table imported using \code{import_data}.
#' @param start_year numeric.
#' @param end_year numeric.
#' @param box numeric vector. Bounding box in the form: (xmin, ymin, xmax, ymax).
#' @return a data.table with the subsetted data sets
#' @export

subset_data <- function(x, start_year, end_year, box){
  if (!is(x, "pRecipe")) stop("Error: x must be a pRecipe data.table")
  x <- x[year(Z) >= start_year & year(Z) <= end_year & x >= box[1] & x <= box[3] & y >= box[2] & y <= box[4]]
  return(x)
}