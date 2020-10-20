#' 20CR data reader
#'
#' Function for reading 20CR NC files.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @return a brick with monthly precipitation data in [mm] and missing values are NA.
#' @export

import_20cr <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip[precip < 0] <- NA
  return(precip)
}

#' CMAP data reader
#'
#' Function for reading CMAP NC files.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @return a brick with monthly precipitation data in [mm/day] and missing values are NA.
#' @export

import_cmap <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip[precip < 0] <- NA
  return(precip)
}

#' CPC data reader
#'
#' Function for reading CPC-GLOBAL NC files.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @return a list of bricks with daily precipitation data in [mm] and missing values are NA.
#' @export

import_cpc <- function(folder_path, chk = FALSE){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  if (chk == TRUE) {
    # use 2 cores in CRAN/Travis/AppVeyor
    no_cores <- 2L
  } else {
    # use all cores in devtools::test()
    no_cores <- detectCores() -1
  }
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, file_name, function(year){
    dummie_brick <- raster::brick(year)
    dummie_brick[dummie_brick < 0] <- NA
    return(dummie_brick)
  })
  stopCluster(cluster)
  return(precip)
}

#' CRU data reader
#'
#' Function for reading CRU_TS NC.GZ file.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @return a brick with monthly precipitation data in [mm/day] and missing values are NA.
#' @export

import_cru <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- gunzip(file_name, temporary = TRUE, remove = FALSE) %>% brick()
  precip[precip < 0] <- NA
  return(precip)
}

#' GHCN-M data reader
#'
#' Function for reading GHCN-M NC file.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @return a brick with monthly precipitation data in [mm] and missing values are NA.
#' @export

import_ghcn <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip[precip < 0] <- NA
  return(precip)
}

#' GPCC data reader
#'
#' Function for reading GPCC NC.GZ file.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @return a brick with monthly precipitation data in [mm/month] and missing values are NA.
#' @export



import_gpcc <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- gunzip(file_name, temporary = TRUE, remove = FALSE) %>% brick()
  precip[precip < 0] <- NA
  return(precip)
}

#' GPCP data reader
#'
#' Function for reading GPCP NC file.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @return a brick with monthly precipitation data in [mm/day] and missing values are NA.
#' @export

import_gpcp <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip[precip < 0] <- NA
  return(precip)
}

#' GPM data reader
#'
#' Function for reading GPM HDF5 files.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @return a list of bricks with monthly precipitation data in [mm/day] and missing values are NA.
#' @export


import_gpm <- function(folder_path, chk = FALSE){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  if (chk == TRUE) {
    # use 2 cores in CRAN/Travis/AppVeyor
    no_cores <- 2L
  } else {
    # use all cores in devtools::test()
    no_cores <- detectCores() -1
  }
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, file_name, function(year){
    dummie_brick <- rhdf5::h5read(year, name = "/Grid/precipitation")
    dummie_brick <- raster::brick(dummie_brick, xmn = -180, xmx = 180, ymn = -90, ymx = 90, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
    dummie_brick <- raster::flip(dummie_brick, direction = "y")
    dummie_brick <- dummie_brick * 24
    dummie_brick[dummie_brick < 0] <- NA
    return(dummie_brick)
  })
  stopCluster(cluster)
  return(precip)
}

#' NCEP/NCAR data reader
#'
#' Function for reading NCEP/NCAR NC files.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @return a brick with monthly precipitation data in [mm/day] and missing values are NA.
#' @export

import_ncep <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip <- precip * 86400
  precip[precip < 0] <- NA
  return(precip)
}

#' NCEP/DOE data reader
#'
#' Function for reading NCEP/DOE NC files.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @return a brick with monthly precipitation data in [mm/day] and missing values are NA.
#' @export

import_ncep2 <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip <- precip * 86400
  precip[precip < 0] <- NA
  return(precip)
}

#' PRECL data reader
#'
#' Function for reading PRECL NC file.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @return a brick with monthly precipitation data in [mm/day] and missing values are NA.
#' @export

import_precl <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip[precip < 0] <- NA
  return(precip)
}

#' TRMM data reader
#'
#' Function for reading TRMM 3B43 HDF files.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @return a list of bricks with monthly precipitation data in [mm/day] and missing values are NA.
#' @export

import_trmm <- function(folder_path, chk = FALSE){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  if (chk == TRUE) {
    # use 2 cores in CRAN/Travis/AppVeyor
    no_cores <- 2L
  } else {
    # use all cores in devtools::test()
    no_cores <- detectCores() -1
  }
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, file_name, function(year){
    dummie_brick <- gdalUtils::get_subdatasets(year)
    dummie_brick <- rgdal::readGDAL(dummie_brick[1])
    dummie_brick <- raster::brick(dummie_brick)
    dummie_brick <- raster::t(dummie_brick)
    sp::proj4string(dummie_brick) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
    raster::extent(dummie_brick) <- c(-180, 180, -50, 50)
    dummie_brick <- raster::flip(dummie_brick, direction = "y")
    dummie_brick <- dummie_brick * 24
    dummie_brick[dummie_brick < 0] <- NA
    return(dummie_brick)
  })
  stopCluster(cluster)
  return(precip)
}

#' UDEL data reader
#'
#' Function for reading UDEL NC file.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @return a brick with monthly precipitation data in [mm] and missing values are NA.
#' @export

import_udel <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip <- precip * 10
  precip[precip < 0] <- NA
  return(precip)
}