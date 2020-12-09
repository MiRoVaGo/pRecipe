#' 20CR data reformatter
#'
#' Function for reading 20CR NC files, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.

reformat_20cr <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  folder_path <- paste0(folder_path, "/20cr")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_list <- brick(file_name) %>% as.list()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, dummie_list, function(year){
    dummie_table <- raster::as.data.frame(year, xy = TRUE, long = TRUE)
    dummie_table <- data.table::as.data.table(dummie_table)
    dummie_table$Z <- as.Date(dummie_table$Z)
    return(dummie_table)
  })
  stopCluster(cluster)
  precip <- data.table::rbindlist(precip)
  dummie_table$name <- "20cr"
  data.table::setkey(dummie_table, name)
  saveRDS(precip, paste0(folder_path, "/../../database/20cr.Rds"))
}

#' CMAP data reformatter
#'
#' Function for reading CMAP NC files, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.

reformat_cmap <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  folder_path <- paste0(folder_path, "/cmap")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_list <- brick(file_name) %>% as.list()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, dummie_list, function(year){
    dummie_table <- raster::as.data.frame(year, xy = TRUE, long = TRUE)
    dummie_table <- data.table::as.data.table(dummie_table)
    dummie_table$Z <- as.Date(dummie_table$Z)
    dummie_table$value <- dummie_table$value * lubridate::days_in_month(dummie_table$Z)
    return(dummie_table)
  })
  stopCluster(cluster)
  precip <- data.table::rbindlist(precip)
  precip$name <- "cmap"
  setkey(precip, name)
  saveRDS(precip, paste0(folder_path, "/../../database/cmap.Rds"))
}

#' CPC data reformatter
#'
#' Function for reading CPC-GLOBAL NC files, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.

reformat_cpc <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  folder_path <- paste0(folder_path, "/cpc")
  file_name <- list.files(folder_path, full.names = TRUE) %>% as.list()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, file_name, function(year){
    dummie_brick <- raster::brick(year)
    layer_names <- as.Date(names(dummie_brick), format = "X%Y.%m.%d")
    layer_names <- c(layer_names[1], layer_names[length(layer_names)])
    layer_names <- seq(layer_names[1], layer_names[2], 'month')
    dummie_brick <- raster::zApply(dummie_brick, by = data.table::month, fun = sum, na.rm = TRUE)
    names(dummie_brick) <- layer_names
    dummie_brick <- raster::as.data.frame(dummie_brick, xy = TRUE, long = TRUE)
    dummie_brick <- data.table::as.data.table(dummie_brick)
    return(dummie_brick)
  })
  stopCluster(cluster)
  precip <- rbindlist(precip)
  precip$name <- "cpc"
  setkey(precip, name)
  saveRDS(precip, paste0(folder_path, "/../../database/cpc.Rds"))
}

#' CRU data reformatter
#'
#' Function for reading CRU_TS NC.GZ file, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.

reformat_cru_ts <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  folder_path <- paste0(folder_path, "/cru_ts")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_list <- gunzip(file_name, remove = FALSE) %>% brick() %>% as.list()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, dummie_list, function(year){
    dummie_table <- raster::as.data.frame(year, xy = TRUE, long = TRUE)
    dummie_table <- data.table::as.data.table(dummie_table)
    dummie_table$Z <- as.Date(dummie_table$Z)
    return(dummie_table)
  })
  stopCluster(cluster)
  precip <- data.table::rbindlist(precip)
  precip$name <- "cru_ts"
  setkey(precip, name)
  saveRDS(precip, paste0(folder_path, "/../../database/cru_ts.Rds"))
}

#' GHCN-M data reformatter
#'
#' Function for reading GHCN-M NC file, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.

reformat_ghcn <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  folder_path <- paste0(folder_path, "/ghcn")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_list <- brick(file_name) %>% as.list()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, dummie_list, function(year){
    dummie_table <- raster::as.data.frame(year, xy = TRUE, long = TRUE)
    dummie_table <- data.table::as.data.table(dummie_table)
    dummie_table$Z <- as.Date(dummie_table$Z)
    return(dummie_table)
  })
  stopCluster(cluster)
  precip <- data.table::rbindlist(precip)
  precip$name <- "ghcn"
  setkey(precip, name)
  saveRDS(precip, paste0(folder_path, "/../../database/ghcn.Rds"))
}

#' GPCC data reformatter
#'
#' Function for reading GPCC NC file, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.

reformat_gpcc <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  folder_path <- paste0(folder_path, "/gpcc")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_list <- brick(file_name) %>% as.list()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, dummie_list, function(year){
    dummie_table <- raster::as.data.frame(year, xy = TRUE, long = TRUE)
    dummie_table <- data.table::as.data.table(dummie_table)
    dummie_table$Z <- as.Date(dummie_table$Z)
    return(dummie_table)
  })
  stopCluster(cluster)
  precip <- data.table::rbindlist(precip)
  precip$name <- "gpcc"
  setkey(precip, name)
  saveRDS(precip, paste0(folder_path, "/../../database/gpcc.Rds"))
}

#' GPCP data reformatter
#'
#' Function for reading GPCP NC file, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.

reformat_gpcp <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  folder_path <- paste0(folder_path, "/gpcp")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_list <- brick(file_name) %>% as.list()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, dummie_list, function(year){
    dummie_table <- raster::as.data.frame(year, xy = TRUE, long = TRUE)
    dummie_table <- data.table::as.data.table(dummie_table)
    dummie_table$Z <- as.Date(dummie_table$Z)
    dummie_table$value <- dummie_table$value * lubridate::days_in_month(dummie_table$Z)
    return(dummie_table)
  })
  stopCluster(cluster)
  precip <- data.table::rbindlist(precip)
  precip$name <- "gpcp"
  setkey(precip, name)
  saveRDS(precip, paste0(folder_path, "/../../database/gpcp.Rds"))
}

#' GPM data reformatter
#'
#' Function for reading GPM HDF5 files, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.

reformat_gpm_imergm <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!requireNamespace("rhdf5", quietly = TRUE)){
    if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager") 
    BiocManager::install("rhdf5")
  }
  folder_path <- paste0(folder_path, "/gpm_imergm")
  file_name <- list.files(folder_path, full.names = TRUE)
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, file_name, function(year){
    layer_name <- sub(".*3IMERG.", "", year)
    layer_name <- substr(layer_name, 1, 8)
    dummie_brick <- rhdf5::h5read(year, name = "/Grid/precipitation")
    dummie_brick <- raster::brick(dummie_brick, xmn = -180, xmx = 180, ymn = -90, ymx = 90, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
    dummie_brick <- raster::flip(dummie_brick, direction = "y")
    dummie_brick[dummie_brick < 0] <- NA
    dummie_brick <- raster::aggregate(dummie_brick, fact = 5, fun = sum, na.rm = TRUE)
    names(dummie_brick) <- layer_name
    dummie_brick <- raster::as.data.frame(dummie_brick, xy = TRUE, long = TRUE)
    data.table::setnames(dummie_brick, "layer", "Z")
    dummie_brick$Z <- as.Date(dummie_brick$Z, format = "X%Y%m%d")
    dummie_brick$value <- dummie_brick$value * lubridate::days_in_month(dummie_brick$Z) * 24
    return(dummie_brick)
  })
  stopCluster(cluster)
  precip <- data.table::rbindlist(precip)
  precip$name <- "gpm_imergm"
  setkey(precip, name)
  saveRDS(precip, paste0(folder_path, "/../../database/gpm_imergm.Rds"))
}

#' NCEP/NCAR data reformatter
#'
#' Function for reading NCEP/NCAR NC files, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.

reformat_ncep_ncar <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  folder_path <- paste0(folder_path, "/ncep_ncar")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_list <- brick(file_name) %>% as.list()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, dummie_list, function(year){
    dummie_table <- raster::as.data.frame(year, xy = TRUE, long = TRUE)
    dummie_table <- data.table::as.data.table(dummie_table)
    dummie_table$Z <- as.Date(dummie_table$Z)
    dummie_table$value <- dummie_table$value * lubridate::days_in_month(dummie_table$Z) * 86400
    return(dummie_table)
  })
  stopCluster(cluster)
  precip <- data.table::rbindlist(precip)
  precip$name <- "ncep_ncar"
  setkey(precip, name)
  saveRDS(precip, paste0(folder_path, "/../../database/ncep_ncar.Rds"))
}

#' NCEP/DOE data reformatter
#'
#' Function for reading NCEP/DOE NC files, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.

reformat_ncep_doe <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  folder_path <- paste0(folder_path, "/ncep_doe")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_list <- brick(file_name) %>% as.list()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, dummie_list, function(year){
    dummie_table <- raster::as.data.frame(year, xy = TRUE, long = TRUE)
    dummie_table <- data.table::as.data.table(dummie_table)
    dummie_table$Z <- as.Date(dummie_table$Z)
    dummie_table$value <- dummie_table$value * lubridate::days_in_month(dummie_table$Z) * 86400
    return(dummie_table)
  })
  stopCluster(cluster)
  precip <- data.table::rbindlist(precip)
  precip$name <- "ncep_doe"
  setkey(precip, name)
  saveRDS(precip, paste0(folder_path, "/../../database/ncep_doe.Rds"))
}

#' PRECL data reformatter
#'
#' Function for reading PRECL NC file, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.

reformat_precl <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be character string.")
  folder_path <- paste0(folder_path, "/precl")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_list <- brick(file_name) %>% as.list()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, dummie_list, function(year){
    dummie_table <- raster::as.data.frame(year, xy = TRUE, long = TRUE)
    dummie_table <- data.table::as.data.table(dummie_table)
    dummie_table$Z <- as.Date(dummie_table$Z)
    dummie_table$value <- dummie_table$value * lubridate::days_in_month(dummie_table$Z)
    return(dummie_table)
  })
  stopCluster(cluster)
  precip <- data.table::rbindlist(precip)
  precip$name <- "precl"
  setkey(precip, name)
  saveRDS(precip, paste0(folder_path, "/../../database/precl.Rds"))
}

#' TRMM data reformatter
#'
#' Function for reading TRMM 3B43 HDF files, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.

reformat_trmm_3b43 <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  folder_path <- paste0(folder_path, "/trmm_3b43")
  file_name <- list.files(folder_path, full.names = TRUE)
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, file_name, function(year){
    layer_name <- sub(".*3B43.", "", year)
    layer_name <- substr(layer_name, 1, 8)
    dummie_brick <- gdalUtils::get_subdatasets(year)
    dummie_brick <- rgdal::readGDAL(dummie_brick[1])
    dummie_brick <- raster::brick(dummie_brick)
    dummie_brick <- raster::t(dummie_brick)
    sp::proj4string(dummie_brick) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
    raster::extent(dummie_brick) <- c(-180, 180, -50, 50)
    dummie_brick <- raster::flip(dummie_brick, direction = "y")
    dummie_brick[dummie_brick < 0] <- NA
    dummie_brick <- raster::aggregate(dummie_brick, fact = 2, fun = sum, na.rm = TRUE)
    names(dummie_brick) <- layer_name
    dummie_brick <- raster::as.data.frame(dummie_brick, xy = TRUE, long = TRUE)
    data.table::setnames(dummie_brick, "layer", "Z")
    dummie_brick$Z <- as.Date(dummie_brick$Z, format = "X%Y%m%d")
    dummie_brick$value <- dummie_brick$value * lubridate::days_in_month(dummie_brick$Z) * 24
    return(dummie_brick)
  })
  stopCluster(cluster)
  precip <- data.table::rbindlist(precip)
  precip$name <- "trmm_3b43"
  setkey(precip, name)
  saveRDS(precip, paste0(folder_path, "/../../database/trmm_3b43.Rds"))
}

#' UDEL data reformatter
#'
#' Function for reading UDEL NC file, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.

reformat_udel <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  folder_path <- paste0(folder_path, "/udel")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_list <- brick(file_name) %>% as.list()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, dummie_list, function(year){
    dummie_table <- raster::as.data.frame(year, xy = TRUE, long = TRUE)
    dummie_table <- data.table::as.data.table(dummie_table)
    dummie_table$Z <- as.Date(dummie_table$Z)
    dummie_table$value <- dummie_table$value * 10
    return(dummie_table)
  })
  stopCluster(cluster)
  precip <- data.table::rbindlist(precip)
  precip$name <- "udel"
  setkey(precip, name)
  saveRDS(precip, paste0(folder_path, "/../../database/udel.Rds"))
}

#' All data reformatter
#'
#' Function for reformatting all of the available data sets.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.

reformat_all <- function(folder_path){
  reformat_20cr(folder_path)
  reformat_cmap(folder_path)
  reformat_cpc(folder_path)
  reformat_cru_ts(folder_path)
  reformat_ghcn(folder_path)
  reformat_gpcc(folder_path)
  reformat_gpcp(folder_path)
  reformat_gpm_imergm(folder_path)
  reformat_ncep_ncar(folder_path)
  reformat_ncep_doe(folder_path)
  reformat_precl(folder_path)
  reformat_trmm_3b43(folder_path)
  reformat_udel(folder_path)
}