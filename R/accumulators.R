#' CMAP data reader
#'
#' Function for reading CMAP NC files.
#'
#' @param file a character string with the path where the data set file is located.
#' @param overwrite logical. If TRUE (default) the original file with precipitation data will be replaced with a file with total precipitation.
#' @return a brick with total monthly precipitation in [mm] at 2.5 degrees for 1979-2019.

prate_to_psum_cmap <- function(file, overwrite){
  if (!is.character(file)) stop ("file should be a character string.")
  precip <- readRDS(file)
  layer_days <- names(precip) %>% substr(2,11) %>% as.Date(format = "%Y.%m.%d") %>% days_in_month()
  precip <- precip * layer_days
  return(precip)
  if (overwrite == TRUE) saveRDS(precip, file)
}

#' GPCP data reader
#'
#' Function for reading GPCP NC file.
#'
#' @param file a character string with the path where the data set file is located.
#' @param overwrite logical. If TRUE (default) the original file with precipitation data will be replaced with a file with total precipitation.
#' @return a brick with total monthly precipitation in [mm] at 2.5 degrees for 1979-2019.

prate_to_psum_gpcp <- function(file, overwrite){
  if (!is.character(file)) stop ("file should be a character string.")
  precip <- readRDS(file)
  layer_days <- names(precip) %>% substr(2,11) %>% as.Date(format = "%Y.%m.%d") %>% days_in_month()
  precip <- precip * layer_days
  return(precip)
  if (overwrite == TRUE) saveRDS(precip, file)
}

#' GPM data reader
#'
#' Function for reading GPM HDF5 files.
#'
#' @param file a character string with the path where the data set file is located.
#' @param overwrite logical. If TRUE (default) the original file with precipitation data will be replaced with a file with total precipitation.
#' @param check logical. If TRUE parallel works only with 2 cores for CRAN tests.
#' @return a list of bricks with monthly precipitation rate in [mm/hour] at 0.1 degrees for 2000-2019.

prate_to_psum_gpm <- function(file, overwrite, check = FALSE){
  if (!is.character(file)) stop ("file should be a character string.")
  precip <- readRDS(file) %>% stack()
  layer_days <- seq(as.Date("2000/6/1"), as.Date("2019/12/1"), by = "month") %>% days_in_month()
  precip <- precip * layer_days * 24
  return(precip)
  if (overwrite == TRUE) saveRDS(precip, file)
}

#' NCEP/NCAR data reader
#'
#' Function for reading NCEP/NCAR NC files.
#'
#' @param file a character string with the path where the data set file is located.
#' @param overwrite logical. If TRUE (default) the original file with precipitation data will be replaced with a file with total precipitation.
#' @return a brick with monthly precipitation rate in [mm/s] at T62 Gaussian grid for 1948-2019.

prate_to_psum_ncep <- function(file, overwrite){
  if (!is.character(file)) stop ("file should be a character string.")
  if (!is.character(file)) stop ("file should be a character string.")
  precip <- readRDS(file)
  layer_days <- names(precip) %>% substr(2,11) %>% as.Date(format = "%Y.%m.%d") %>% days_in_month()
  precip <- precip * layer_days *86400
  return(precip)
  if (overwrite == TRUE) saveRDS(precip, file)
}

#' NCEP/DOE data reader
#'
#' Function for reading NCEP/DOE NC files.
#'
#' @param file a character string with the path where the data set file is located.
#' @param overwrite logical. If TRUE (default) the original file with precipitation data will be replaced with a file with total precipitation.
#' @return a brick with monthly precipitation rate in [mm/s] at T62 Gaussian grid for 1979-2019.

prate_to_psum_ncep2 <- function(file, overwrite){
  if (!is.character(file)) stop ("file should be a character string.")
  if (!is.character(file)) stop ("file should be a character string.")
  precip <- readRDS(file)
  layer_days <- names(precip) %>% substr(2,11) %>% as.Date(format = "%Y.%m.%d") %>% days_in_month()
  precip <- precip * layer_days * 86400
  return(precip)
  if (overwrite == TRUE) saveRDS(precip, file)
}

#' PRECL data reader
#'
#' Function for reading PRECL NC file.
#'
#' @param file a character string with the path where the data set file is located.
#' @param overwrite logical. If TRUE (default) the original file with precipitation data will be replaced with a file with total precipitation.
#' @return a brick with monthly precipitation rate in [mm/day] at 0.5 degrees for 1948-2012.

prate_to_psum_precl <- function(file, overwrite){
  if (!is.character(file)) stop ("file should be a character string.")
  precip <- readRDS(file)
  layer_days <- names(precip) %>% substr(2,11) %>% as.Date(format = "%Y.%m.%d") %>% days_in_month()
  precip <- precip * layer_days
  return(precip)
  if (overwrite == TRUE) saveRDS(precip, file)
}

#' TRMM data reader
#'
#' Function for reading TRMM 3B43 HDF files.
#'
#' @param file a character string with the path where the data set file is located.
#' @param overwrite logical. If TRUE (default) the original file with precipitation data will be replaced with a file with total precipitation.
#' @param check logical. If TRUE parallel works only with 2 cores for CRAN tests.
#' @return a list of bricks with monthly precipitation rate in [mm/h] at 0.25 degrees for 1998-2019.

prate_to_psum_trmm <- function(file, overwrite, check = FALSE){
  if (!is.character(file)) stop ("file should be a character string.")
  precip <- readRDS(file) %>% stack()
  layer_days <- seq(as.Date("2000/6/1"), as.Date("2019/12/1"), by = "month") %>% days_in_month()
  precip <- precip * layer_days * 24
  return(precip)
  if (overwrite == TRUE) saveRDS(precip, file)
}

#' UDEL data reader
#'
#' Function for reading UDEL NC file.
#'
#' @param file a character string with the path where the data set file is located.
#' @param overwrite logical. If TRUE (default) the original file with precipitation data will be replaced with a file with total precipitation.
#' @return a brick with total monthly precipitation in [cm] at 0.5 degrees for 1900-2017.

prate_to_psum_udel <- function(file, overwrite){
  if (!is.character(file)) stop ("file should be a character string.")
  precip <- readRDS(file)
  precip <- precip * 10
  return(precip)
  if (overwrite == TRUE) saveRDS(precip, file)
}