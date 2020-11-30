#' 20CR data reader
#'
#' Function for reading 20CR NC files.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @param save logical. If TRUE (default) an .Rds file will be saved in the same location.
#' @param preserve logical. If TRUE (default) the original file will be preserved.
#' @return a brick with total monthly precipitation in [mm] at 1 degrees for 1863-2015.

reformat_20cr <- function(folder_path, save, preserve){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip[precip < 0] <- NA
  return(precip)
  if (preserve == FALSE) file.remove(file_name)
  if (save == TRUE) saveRDS(precip, paste0(folder_path, "/20cr_M_1_186301_201512.Rds"))
}

#' CMAP data reader
#'
#' Function for reading CMAP NC files.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @param save logical. If TRUE (default) an .Rds file will be saved in the same location.
#' @param preserve logical. If TRUE (default) the original file will be preserved.
#' @return a brick with monthly precipitation rate in [mm/day] at 2.5 degrees for 1979-2019.

reformat_cmap <- function(folder_path, save, preserve){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip[precip < 0] <- NA
  return(precip)
  if (preserve == FALSE) file.remove(file_name)
  if (save == TRUE) saveRDS(precip, paste0(folder_path, "/cmap_M_25_197901_201912.Rds"))
}

#' CPC data reader
#'
#' Function for reading CPC-GLOBAL NC files.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @param save logical. If TRUE (default) an .Rds file will be saved in the same location.
#' @param preserve logical. If TRUE (default) the original file will be preserved.
#' @param check logical. If TRUE parallel works only with 2 cores for CRAN tests.
#' @return a list of bricks with total daily precipitation in [mm] at 0.5 degrees for 1979-2019.

reformat_cpc <- function(folder_path, save, preserve, check = FALSE){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  if (check == TRUE) {
    no_cores <- 2L
  } else {
    no_cores <- detectCores() -1
  }
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- vector(mode = "list", length = length(file_name))
  precip <- parLapply(cluster, file_name, function(year){
    dummie_brick <- raster::brick(year)
    dummie_brick[dummie_brick < 0] <- NA
    return(dummie_brick)
  })
  stopCluster(cluster)
  return(precip)
  if (preserve == FALSE) file.remove(paste0(folder_path, "/*"))
  if (save == TRUE) saveRDS(precip, paste0(folder_path, "/cpc_d_05_197901_201912.Rds"))
}

#' CRU data reader
#'
#' Function for reading CRU_TS NC.GZ file.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @param save logical. If TRUE (default) an .Rds file will be saved in the same location.
#' @param preserve logical. If TRUE (default) the original file will be preserved.
#' @return a brick with monthly precipitation rate in [mm/month] at 0.5 degrees for 1901-2019.

reformat_cru <- function(folder_path, save, preserve){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- gunzip(file_name, remove = FALSE) %>% brick()
  precip[precip < 0] <- NA
  return(precip)
  if (preserve == FALSE) file.remove(file_name)
  if (save == TRUE) saveRDS(precip, paste0(folder_path, "/cru_M_05_190101_201912.Rds"))
}

#' GHCN-M data reader
#'
#' Function for reading GHCN-M NC file.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @param save logical. If TRUE (default) an .Rds file will be saved in the same location.
#' @param preserve logical. If TRUE (default) the original file will be preserved.
#' @return a brick with total monthly precipitation in [mm] at 5 degrees for 1900-2015.

reformat_ghcn <- function(folder_path, save, preserve){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip[precip < 0] <- NA
  return(precip)
  if (preserve == FALSE) file.remove(file_name)
  if (save == TRUE) saveRDS(precip, paste0(folder_path, "/ghcn_M_5_190001_201505.Rds"))
}

#' GPCC data reader
#'
#' Function for reading GPCC NC file.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @param save logical. If TRUE (default) an .Rds file will be saved in the same location.
#' @param preserve logical. If TRUE (default) the original file will be preserved.
#' @return a brick with total monthly precipitation in [mm] at 0.5 degrees for 1891-2016.

reformat_gpcc <- function(folder_path, save, preserve){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip[precip < 0] <- NA
  return(precip)
  if (preserve == FALSE) file.remove(file_name)
  if (save == TRUE) saveRDS(precip, paste0(folder_path, "/gpcc_M_05_189101_201612.Rds"))
}

#' GPCP data reader
#'
#' Function for reading GPCP NC file.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @param save logical. If TRUE (default) an .Rds file will be saved in the same location.
#' @param preserve logical. If TRUE (default) the original file will be preserved.
#' @return a brick with monthly precipitation rate in [mm/day] at 2.5 degrees for 1979-2019.

reformat_gpcp <- function(folder_path, save, preserve){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip[precip < 0] <- NA
  return(precip)
  if (preserve == FALSE) file.remove(file_name)
  if (save == TRUE) saveRDS(precip, paste0(folder_path, "/gpcp_M_25_197901_201912.Rds"))
}

#' GPM data reader
#'
#' Function for reading GPM HDF5 files.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @param save logical. If TRUE (default) an .Rds file will be saved in the same location.
#' @param preserve logical. If TRUE (default) the original file will be preserved.
#' @param check logical. If TRUE parallel works only with 2 cores for CRAN tests.
#' @return a list of bricks with monthly precipitation rate in [mm/hour] at 0.1 degrees for 2000-2019.

reformat_gpm <- function(folder_path, save, preserve, check = FALSE){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!requireNamespace("rhdf5", quietly = TRUE)){
    if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager") 
    BiocManager::install("rhdf5")
  }
  file_name <- list.files(folder_path, full.names = TRUE)
  if (check == TRUE) {
    no_cores <- 2L
  } else {
    no_cores <- detectCores() -1
  }
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, file_name, function(year){
    dummie_brick <- rhdf5::h5read(year, name = "/Grid/precipitation")
    dummie_brick <- raster::brick(dummie_brick, xmn = -180, xmx = 180, ymn = -90, ymx = 90, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
    dummie_brick <- raster::flip(dummie_brick, direction = "y")
    dummie_brick[dummie_brick < 0] <- NA
    return(dummie_brick)
  })
  stopCluster(cluster)
  return(precip)
  if (preserve == FALSE) file.remove(paste0(folder_path, "/*"))
  if (save == TRUE) saveRDS(precip, paste0(folder_path, "/gpm_M_01_200006_201912.Rds"))
}

#' NCEP/NCAR data reader
#'
#' Function for reading NCEP/NCAR NC files.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @param save logical. If TRUE (default) an .Rds file will be saved in the same location.
#' @param preserve logical. If TRUE (default) the original file will be preserved.
#' @return a brick with monthly precipitation rate in [mm/s] at T62 Gaussian grid for 1948-2019.

reformat_ncep <- function(folder_path, save, preserve){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip[precip < 0] <- NA
  return(precip)
  if (preserve == FALSE) file.remove(file_name)
  if (save == TRUE) saveRDS(precip, paste0(folder_path, "/ncep_M_T62_194801_201912.Rds"))
}

#' NCEP/DOE data reader
#'
#' Function for reading NCEP/DOE NC files.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @param save logical. If TRUE (default) an .Rds file will be saved in the same location.
#' @param preserve logical. If TRUE (default) the original file will be preserved.
#' @return a brick with monthly precipitation rate in [mm/s] at T62 Gaussian grid for 1979-2019.

reformat_ncep2 <- function(folder_path, save, preserve){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip[precip < 0] <- NA
  return(precip)
  if (preserve == FALSE) file.remove(file_name)
  if (save == TRUE) saveRDS(precip, paste0(folder_path, "/ncep2_M_T62_197901_201912.Rds"))
}

#' PRECL data reader
#'
#' Function for reading PRECL NC file.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @param save logical. If TRUE (default) an .Rds file will be saved in the same location.
#' @param preserve logical. If TRUE (default) the original file will be preserved.
#' @return a brick with monthly precipitation rate in [mm/day] at 0.5 degrees for 1948-2012.

reformat_precl <- function(folder_path, save, preserve){
  if (!is.character(folder_path)) stop ("folder_path should be character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip[precip < 0] <- NA
  return(precip)
  if (preserve == FALSE) file.remove(file_name)
  if (save == TRUE) saveRDS(precip, paste0(folder_path, "/precl_M_05_194801_201212.Rds"))
}

#' TRMM data reader
#'
#' Function for reading TRMM 3B43 HDF files.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @param save logical. If TRUE (default) an .Rds file will be saved in the same location.
#' @param preserve logical. If TRUE (default) the original file will be preserved.
#' @param check logical. If TRUE parallel works only with 2 cores for CRAN tests.
#' @return a list of bricks with monthly precipitation rate in [mm/h] at 0.25 degrees for 1998-2019.

reformat_trmm <- function(folder_path, save, preserve, check = FALSE){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  if (check == TRUE) {
    no_cores <- 2L
  } else {
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
    dummie_brick[dummie_brick < 0] <- NA
    return(dummie_brick)
  })
  stopCluster(cluster)
  return(precip)
  if (preserve == FALSE) file.remove(paste0(folder_path, "/*"))
  if (save == TRUE) saveRDS(precip, paste0(folder_path, "/trmm_M_025_199801_201912.Rds"))
}

#' UDEL data reader
#'
#' Function for reading UDEL NC file.
#'
#' @param folder_path a character string with the path where the data set file is located.
#' @param save logical. If TRUE (default) an .Rds file will be saved in the same location.
#' @param preserve logical. If TRUE (default) the original file will be preserved.
#' @return a brick with total monthly precipitation in [cm] at 0.5 degrees for 1900-2017.

reformat_udel <- function(folder_path, save, preserve){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip[precip < 0] <- NA
  return(precip)
  if (preserve == FALSE) file.remove(file_name)
  if (save == TRUE) saveRDS(precip, paste0(folder_path, "/udel_M_05_190001_201712.Rds"))
}