#' 20CR data reformatter
#'
#' Function for reading 20CR NC files, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path to the "raw" folder.
#' @export

reformat_20cr <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!grepl("*/data/raw", folder_path)){
    stop("Error: folder_path should point to the location of 'data/raw'")
  }
  folder_path <- paste0(folder_path, "/20cr")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_raster <- raster::brick(file_name)
  dummie_years <- names(dummie_raster) %>% as.Date(format = "X%Y.%m.%d") %>% year() %>% unique()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  for (dummie_layers in dummie_years) {
    dummie_list <- raster::subset(dummie_raster, grep(dummie_layers, names(dummie_raster), value = TRUE)) %>% raster::as.list()
    precip <- parLapply(cluster, dummie_list, function(year){
      year <- raster::setZ(year, as.Date(names(year), format = "X%Y.%m.%d"))
      dummie_table <- raster::disaggregate(year, fact = raster::res(year)/0.5)
      dummie_table[dummie_table < 0] <- NA
      dummie_table <- raster::as.data.frame(dummie_table, xy = TRUE, long = TRUE, na.rm = TRUE)
      dummie_table <- data.table::as.data.table(dummie_table)
      dummie_table$layer <- as.Date(dummie_table$layer, format = "X%Y.%m.%d")
      data.table::setnames(dummie_table, "layer", "Z")
      dummie_table$value <- dummie_table$value * lubridate::days_in_month(dummie_table$Z) * (24/3)
      return(dummie_table)
    })
    precip <- data.table::rbindlist(precip)
    precip[x > 180, x := x - 360]
    precip$name <- "20cr"
    class(precip) <- append(class(precip),"pRecipe")
    saveRDS(precip[(y >= -90) & (y <= 90)], paste0(folder_path, "/../../database/20cr_", dummie_layers, ".Rds"))
  }
  stopCluster(cluster)
}

#' CMAP data reformatter
#'
#' Function for reading CMAP NC files, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path to the "raw" folder.
#' @export

reformat_cmap <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!grepl("*/data/raw", folder_path)){
    stop("Error: folder_path should point to the location of 'data/raw'")
  }
  folder_path <- paste0(folder_path, "/cmap")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_raster <- raster::brick(file_name)
  dummie_years <- names(dummie_raster) %>% as.Date(format = "X%Y.%m.%d") %>% year() %>% unique()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  for (dummie_layers in dummie_years) {
    dummie_list <- raster::subset(dummie_raster, grep(dummie_layers, names(dummie_raster), value = TRUE)) %>% raster::as.list()
    precip <- parLapply(cluster, dummie_list, function(year){
      year <- raster::setZ(year, as.Date(names(year), format = "X%Y.%m.%d"))
      dummie_table <- raster::disaggregate(year, fact = raster::res(year)/0.5)
      dummie_table[dummie_table < 0] <- NA
      dummie_table <- raster::as.data.frame(dummie_table, xy = TRUE, long = TRUE, na.rm = TRUE)
      dummie_table <- data.table::as.data.table(dummie_table)
      dummie_table$layer <- as.Date(dummie_table$layer, format = "X%Y.%m.%d")
      data.table::setnames(dummie_table, "layer", "Z")
      dummie_table$value <- dummie_table$value * lubridate::days_in_month(dummie_table$Z)
      return(dummie_table)
    })
    precip <- data.table::rbindlist(precip)
    precip[x > 180, x := x - 360]
    precip$name <- "cmap"
    class(precip) <- append(class(precip),"pRecipe")
    saveRDS(precip, paste0(folder_path, "/../../database/cmap_", dummie_layers, ".Rds"))
  }
  stopCluster(cluster)
}

#' CMORPH data reformatter
#'
#' Function for reading CMORPH NC files, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path to the "raw" folder.
#' @export

reformat_cmorph <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!grepl("*/data/raw", folder_path)){
    stop("Error: folder_path should point to the location of 'data/raw'")
  }
  folder_path <- paste0(folder_path, "/cmorph")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_raster <- raster::brick(file_name)
  dummie_years <- names(dummie_raster) %>% as.Date(format = "X%Y.%m.%d") %>% year() %>% unique()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  for (dummie_layers in dummie_years) {
    dummie_list <- raster::subset(dummie_raster, grep(dummie_layers, names(dummie_raster), value = TRUE)) %>% raster::as.list()
    precip <- parLapply(cluster, dummie_list, function(year){
      year <- raster::setZ(year, as.Date(names(year), format = "X%Y.%m.%d"))
      dummie_table <- raster::zApply(year, by = data.table::month, fun = sum, na.rm = TRUE)
      dummie_table <- raster::aggregate(dummie_table, fact = 2, fun = mean, na.rm = TRUE)
      dummie_table[dummie_table < 0] <- NA
      dummie_table <- raster::as.data.frame(dummie_table, xy = TRUE, long = TRUE, na.rm = TRUE)
      dummie_table <- data.table::as.data.table(dummie_table)
      dummie_table$layer <- as.Date(dummie_table$layer, format = "X%Y.%m.%d")
      data.table::setnames(dummie_table, "layer", "Z")
      return(dummie_table)
    })
    precip <- data.table::rbindlist(precip)
    precip[x > 180, x := x - 360]
    precip$name <- "cmorph"
    class(precip) <- append(class(precip),"pRecipe")
    saveRDS(precip[(y >= -90) & (y <= 90)], paste0(folder_path, "/../../database/cmorph_", dummie_layers, ".Rds"))
  }
  stopCluster(cluster)
}

#' CPC data reformatter
#'
#' Function for reading CPC-GLOBAL NC files, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path to the "raw" folder.
#' @export

reformat_cpc <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!grepl("*/data/raw", folder_path)){
    stop("Error: folder_path should point to the location of 'data/raw'")
  }
  folder_path <- paste0(folder_path, "/cpc")
  dummie_list <- list.files(folder_path, full.names = TRUE) %>% as.list()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  clusterEvalQ(cluster, library("data.table"))
  clusterExport(cluster, "folder_path", envir = environment())
  parLapply(cluster, dummie_list, function(year){
    dummie_table <- raster::brick(year)
    layer_days <- as.Date(names(dummie_table), format = "X%Y.%m.%d")
    layer_months <- c(layer_days[1], layer_days[length(layer_days)])
    layer_months <- seq(layer_months[1], layer_months[2], 'month')
    dummie_table <- raster::zApply(dummie_table, by = data.table::month, fun = sum, na.rm = TRUE)
    dummie_table <- raster::setZ(dummie_table, layer_months)
    dummie_table <- raster::as.data.frame(dummie_table, xy = TRUE, long = TRUE, na.rm = TRUE)
    dummie_table <- data.table::as.data.table(dummie_table)
    dummie_table[x > 180, x := x - 360]
    dummie_table$name <- "cpc"
    class(dummie_table) <- append(class(dummie_table),"pRecipe")
    saveRDS(dummie_table, paste0(folder_path, "/../../database/cpc_", substr(year, nchar(year) - 6, nchar(year) - 3),".Rds"))
  })
  stopCluster(cluster)
}

#' CRU data reformatter
#'
#' Function for reading CRU_TS NC.GZ file, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path to the "raw" folder.
#' @export

reformat_cru_ts <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!grepl("*/data/raw", folder_path)){
    stop("Error: folder_path should point to the location of 'data/raw'")
  }
  folder_path <- paste0(folder_path, "/cru_ts")
  file_name <- list.files(folder_path, full.names = TRUE, pattern = "*.gz")
  dummie_raster <- gunzip(file_name, remove = FALSE, skip = TRUE) %>% raster::brick(varname = "pre")
  dummie_years <- names(dummie_raster) %>% as.Date(format = "X%Y.%m.%d") %>% year() %>% unique()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  for (dummie_layers in dummie_years) {
    dummie_list <- raster::subset(dummie_raster, grep(dummie_layers, names(dummie_raster), value = TRUE)) %>% raster::as.list()
    precip <- parLapply(cluster, dummie_list, function(dummie_table){
      dummie_table <- raster::setZ(dummie_table, as.Date(names(dummie_table), format = "X%Y.%m.%d"))
      dummie_table <- raster::as.data.frame(dummie_table, xy = TRUE, long = TRUE, na.rm = TRUE)
      dummie_table <- data.table::as.data.table(dummie_table)
      dummie_table$Z <- as.Date(dummie_table$Z)
      return(dummie_table)
    })
    precip <- data.table::rbindlist(precip)
    lubridate::day(precip$Z) <- 1
    precip$name <- "cru_ts"
    class(precip) <- append(class(precip),"pRecipe")
    saveRDS(precip, paste0(folder_path, "/../../database/cru_ts_", dummie_layers, ".Rds"))
  }
  stopCluster(cluster)
}

#' GHCN-M data reformatter
#'
#' Function for reading GHCN-M NC file, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path to the "raw" folder.
#' @export

reformat_ghcn <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!grepl("*/data/raw", folder_path)){
    stop("Error: folder_path should point to the location of 'data/raw'")
  }
  folder_path <- paste0(folder_path, "/ghcn")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_raster <- raster::brick(file_name)
  dummie_years <- names(dummie_raster) %>% as.Date(format = "X%Y.%m.%d") %>% year() %>% unique()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  for (dummie_layers in dummie_years) {
    dummie_list <- raster::subset(dummie_raster, grep(dummie_layers, names(dummie_raster), value = TRUE)) %>% raster::as.list()
    precip <- parLapply(cluster, dummie_list, function(year){
      year <- raster::setZ(year, as.Date(names(year), format = "X%Y.%m.%d"))
      dummie_table <- raster::disaggregate(year, fact = raster::res(year)/0.5)
      dummie_table[dummie_table < 0] <- NA
      dummie_table <- raster::as.data.frame(dummie_table, xy = TRUE, long = TRUE, na.rm = TRUE)
      dummie_table <- data.table::as.data.table(dummie_table)
      dummie_table$layer <- as.Date(dummie_table$layer, format = "X%Y.%m.%d")
      data.table::setnames(dummie_table, "layer", "Z")
      return(dummie_table)
    })
    precip <- data.table::rbindlist(precip)
    precip[x > 180, x := x - 360]
    precip$name <- "ghcn"
    class(precip) <- append(class(precip),"pRecipe")
    saveRDS(precip, paste0(folder_path, "/../../database/ghcn_", dummie_layers, ".Rds"))
  }
  stopCluster(cluster)
}

#' GPCC data reformatter
#'
#' Function for reading GPCC NC file, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path to the "raw" folder.
#' @export

reformat_gpcc <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!grepl("*/data/raw", folder_path)){
    stop("Error: folder_path should point to the location of 'data/raw'")
  }
  folder_path <- paste0(folder_path, "/gpcc")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_raster <- raster::brick(file_name)
  dummie_years <- names(dummie_raster) %>% as.Date(format = "X%Y.%m.%d") %>% year() %>% unique()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  for (dummie_layers in dummie_years) {
    dummie_list <- raster::subset(dummie_raster, grep(dummie_layers, names(dummie_raster), value = TRUE)) %>% raster::as.list()
    precip <- parLapply(cluster, dummie_list, function(year){
      year <- raster::setZ(year, as.Date(names(year), format = "X%Y.%m.%d"))
      dummie_table <- raster::as.data.frame(year, xy = TRUE, long = TRUE, na.rm = TRUE)
      dummie_table <- data.table::as.data.table(dummie_table)
      dummie_table$Z <- as.Date(dummie_table$Z)
      return(dummie_table)
    })
    precip <- data.table::rbindlist(precip)
    precip[x > 180, x := x - 360]
    precip$name <- "gpcc"
    class(precip) <- append(class(precip),"pRecipe")
    saveRDS(precip, paste0(folder_path, "/../../database/gpcc_", dummie_layers, ".Rds"))
  }
  stopCluster(cluster)
}

#' GPCP data reformatter
#'
#' Function for reading GPCP NC file, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path to the "raw" folder.
#' @export

reformat_gpcp <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!grepl("*/data/raw", folder_path)){
    stop("Error: folder_path should point to the location of 'data/raw'")
  }
  folder_path <- paste0(folder_path, "/gpcp")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_raster <- raster::brick(file_name)
  dummie_years <- names(dummie_raster) %>% as.Date(format = "X%Y.%m.%d") %>% year() %>% unique()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  for (dummie_layers in dummie_years) {
    dummie_list <- raster::subset(dummie_raster, grep(dummie_layers, names(dummie_raster), value = TRUE)) %>% raster::as.list()
    precip <- parLapply(cluster, dummie_list, function(year){
      year <- raster::setZ(year, as.Date(names(year), format = "X%Y.%m.%d"))
      dummie_table <- raster::disaggregate(year, fact = raster::res(year)/0.5)
      dummie_table[dummie_table < 0] <- NA
      dummie_table <- raster::as.data.frame(dummie_table, xy = TRUE, long = TRUE, na.rm = TRUE)
      dummie_table <- data.table::as.data.table(dummie_table)
      dummie_table$layer <- as.Date(dummie_table$layer, format = "X%Y.%m.%d")
      data.table::setnames(dummie_table, "layer", "Z")
      dummie_table$value <- dummie_table$value * lubridate::days_in_month(dummie_table$Z)
      return(dummie_table)
    })
    precip <- data.table::rbindlist(precip)
    precip[x > 180, x := x - 360]
    precip$name <- "gpcp"
    class(precip) <- append(class(precip),"pRecipe")
    saveRDS(precip, paste0(folder_path, "/../../database/gpcp_", dummie_layers, ".Rds"))
  }
  stopCluster(cluster)
}

#' GPM data reformatter
#'
#' Function for reading GPM HDF5 files, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path to the "raw" folder.
#' @export

reformat_gpm_imergm <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!grepl("*/data/raw", folder_path)){
    stop("Error: folder_path should point to the location of 'data/raw'")
  }
  folder_path <- paste0(folder_path, "/gpm_imergm")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_years <- sub(".*3IMERG.", "", file_name) %>% substr(1, 4) %>% unique()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  clusterEvalQ(cluster, library("hdf5r"))
  for (dummie_layers in dummie_years) {
    dummie_list <- grep(dummie_layers, file_name, value = TRUE)
    precip <- parLapply(cluster, dummie_list, function(year){
      layer_name <- sub(".*3IMERG.", "", year)
      layer_name <- substr(layer_name, 1, 8)
      dummie_file <- H5File$new(year, mode="r+")
      dummie_table <- dummie_file[["Grid/precipitation"]]
      dummie_table <- dummie_table[1:1800, 1:3600, 1]
      dummie_table[dummie_table < 0] <- NA
      dummie_file$close_all()
      dummie_table <- raster::raster(dummie_table, xmn = -180, xmx = 180, ymn = -90, ymx = 90, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
      dummie_table <- raster::flip(dummie_table, direction = "y")
      dummie_table <- raster::aggregate(dummie_table, fact = 5, fun = mean, na.rm = TRUE)
      names(dummie_table) <- layer_name
      dummie_table <- raster::as.data.frame(dummie_table, xy = TRUE, long = TRUE, na.rm = TRUE)
      data.table::setnames(dummie_table, "layer", "Z")
      dummie_table$Z <- as.Date(dummie_table$Z, format = "X%Y%m%d")
      dummie_table$value <- dummie_table$value * lubridate::days_in_month(dummie_table$Z) * 24
      return(dummie_table)
    })
    precip <- data.table::rbindlist(precip)
    precip$name <- "gpm_imergm"
    class(precip) <- append(class(precip),"pRecipe")
    saveRDS(precip, paste0(folder_path, "/../../database/gpm_imergm_", dummie_layers, ".Rds"))
  }
  stopCluster(cluster)
}

#' NCEP/DOE data reformatter
#'
#' Function for reading NCEP/DOE NC files, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path to the "raw" folder.
#' @export

reformat_ncep_doe <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!grepl("*/data/raw", folder_path)){
    stop("Error: folder_path should point to the location of 'data/raw'")
  }
  folder_path <- paste0(folder_path, "/ncep_doe")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_raster <- raster::brick(file_name)
  dummie_years <- names(dummie_raster) %>% as.Date(format = "X%Y.%m.%d") %>% year() %>% unique()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  for (dummie_layers in dummie_years) {
    dummie_list <- raster::subset(dummie_raster, grep(dummie_layers, names(dummie_raster), value = TRUE)) %>% raster::as.list()
    precip <- parLapply(cluster, dummie_list, function(year){
      year <- raster::setZ(year, as.Date(names(year), format = "X%Y.%m.%d"))
      dummie_raster <- raster::raster(xmn=-0, xmx=360, ymn=-90, ymx=90, ncols=720, nrows=360)
      dummie_raster <- raster::setValues(dummie_raster, 1:(raster::ncell(dummie_raster)))
      dummie_table <- raster::disaggregate(year, fact = round(raster::res(year)/0.5))
      dummie_table[dummie_table < 0] <- NA
      dummie_table <- raster::resample(dummie_table, dummie_raster, method = "bilinear")
      dummie_table <- raster::as.data.frame(dummie_table, xy = TRUE, long = TRUE, na.rm = TRUE)
      dummie_table <- data.table::as.data.table(dummie_table)
      dummie_table$layer <- as.Date(dummie_table$layer, format = "X%Y.%m.%d")
      data.table::setnames(dummie_table, "layer", "Z")
      dummie_table$value <- dummie_table$value * lubridate::days_in_month(dummie_table$Z) * 86400
      return(dummie_table)
    })
    precip <- data.table::rbindlist(precip)
    precip[x > 180, x := x - 360]
    precip$name <- "ncep_doe"
    class(precip) <- append(class(precip),"pRecipe")
    saveRDS(precip, paste0(folder_path, "/../../database/ncep_doe_", dummie_layers, ".Rds"))
  }
  stopCluster(cluster)
}

#' NCEP/NCAR data reformatter
#'
#' Function for reading NCEP/NCAR NC files, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path to the "raw" folder.
#' @export

reformat_ncep_ncar <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!grepl("*/data/raw", folder_path)){
    stop("Error: folder_path should point to the location of 'data/raw'")
  }
  folder_path <- paste0(folder_path, "/ncep_ncar")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_raster <- raster::brick(file_name)
  dummie_years <- names(dummie_raster) %>% as.Date(format = "X%Y.%m.%d") %>% year() %>% unique()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  for (dummie_layers in dummie_years) {
    dummie_list <- raster::subset(dummie_raster, grep(dummie_layers, names(dummie_raster), value = TRUE)) %>% raster::as.list()
    precip <- parLapply(cluster, dummie_list, function(year){
      year <- raster::setZ(year, as.Date(names(year), format = "X%Y.%m.%d"))
      dummie_raster <- raster::raster(xmn=-0, xmx=360, ymn=-90, ymx=90, ncols=720, nrows=360)
      dummie_raster <- raster::setValues(dummie_raster, 1:(raster::ncell(dummie_raster)))
      dummie_table <- raster::disaggregate(year, fact = round(raster::res(year)/0.5))
      dummie_table[dummie_table < 0] <- NA
      dummie_table <- raster::resample(dummie_table, dummie_raster, method = "bilinear")
      dummie_table <- raster::as.data.frame(dummie_table, xy = TRUE, long = TRUE, na.rm = TRUE)
      dummie_table <- data.table::as.data.table(dummie_table)
      dummie_table$layer <- as.Date(dummie_table$layer, format = "X%Y.%m.%d")
      data.table::setnames(dummie_table, "layer", "Z")
      dummie_table$value <- dummie_table$value * lubridate::days_in_month(dummie_table$Z) * 86400
      return(dummie_table)
    })
    precip <- data.table::rbindlist(precip)
    precip[x > 180, x := x - 360]
    precip$name <- "ncep_ncar"
    class(precip) <- append(class(precip),"pRecipe")
    saveRDS(precip, paste0(folder_path, "/../../database/ncep_ncar_", dummie_layers, ".Rds"))
  }
  stopCluster(cluster)
}

#' PERSIANN CDR data reformatter
#'
#' Function for reading PERCIAN CDR NC files, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path to the "raw" folder.
#' @export

reformat_persiann_cdr <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!grepl("*/data/raw", folder_path)){
    stop("Error: folder_path should point to the location of 'data/raw'")
  }
  folder_path <- paste0(folder_path, "/persiann_cdr")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_raster <- raster::brick(file_name)
  dummie_years <- names(dummie_raster) %>% as.Date(format = "X%Y.%m.%d") %>% year() %>% unique()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  for (dummie_layers in dummie_years) {
    dummie_list <- raster::subset(dummie_raster, grep(dummie_layers, names(dummie_raster), value = TRUE)) %>% raster::as.list()
    precip <- parLapply(cluster, dummie_list, function(year){
      year <- raster::setZ(year, as.Date(names(year), format = "X%Y.%m.%d"))
      dummie_table <- raster::zApply(year, by = data.table::month, fun = sum, na.rm = TRUE)
      dummie_table <- raster::aggregate(dummie_table, fact = 2, fun = mean, na.rm = TRUE)
      dummie_table[dummie_table < 0] <- NA
      dummie_table <- raster::as.data.frame(dummie_table, xy = TRUE, long = TRUE, na.rm = TRUE)
      dummie_table <- data.table::as.data.table(dummie_table)
      dummie_table$layer <- as.Date(dummie_table$layer, format = "X%Y.%m.%d")
      data.table::setnames(dummie_table, "layer", "Z")
      return(dummie_table)
    })
    precip <- data.table::rbindlist(precip)
    precip[x > 180, x := x - 360]
    precip$name <- "persiann_cdr"
    class(precip) <- append(class(precip),"pRecipe")
    saveRDS(precip[(y >= -90) & (y <= 90)], paste0(folder_path, "/../../database/persiann_cdr_", dummie_layers, ".Rds"))
  }
  stopCluster(cluster)
}

#' PRECL data reformatter
#'
#' Function for reading PRECL NC file, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path to the "raw" folder.
#' @export

reformat_precl <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be character string.")
  if (!grepl("*/data/raw", folder_path)){
    stop("Error: folder_path should point to the location of 'data/raw'")
  }
  folder_path <- paste0(folder_path, "/precl")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_raster <- raster::brick(file_name)
  dummie_years <- names(dummie_raster) %>% as.Date(format = "X%Y.%m.%d") %>% year() %>% unique()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  for (dummie_layers in dummie_years) {
    dummie_list <- raster::subset(dummie_raster, grep(dummie_layers, names(dummie_raster), value = TRUE)) %>% raster::as.list()
    precip <- parLapply(cluster, dummie_list, function(year){
      year <- raster::setZ(year, as.Date(names(year), format = "X%Y.%m.%d"))
      dummie_table <- raster::as.data.frame(year, xy = TRUE, long = TRUE, na.rm = TRUE)
      dummie_table <- data.table::as.data.table(dummie_table)
      dummie_table$Z <- as.Date(dummie_table$Z)
      dummie_table$value <- dummie_table$value * lubridate::days_in_month(dummie_table$Z)
      return(dummie_table)
    })
    precip <- data.table::rbindlist(precip)
    precip[x > 180, x := x - 360]
    precip$name <- "precl"
    class(precip) <- append(class(precip),"pRecipe")
    saveRDS(precip, paste0(folder_path, "/../../database/precl_", dummie_layers, ".Rds"))
  }
  stopCluster(cluster)
}

#' TRMM data reformatter
#'
#' Function for reading TRMM 3B43 HDF files, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path to the "raw" folder.
#' @export

reformat_trmm_3b43 <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!grepl("*/data/raw", folder_path)){
    stop("Error: folder_path should point to the location of 'data/raw'")
  }
  folder_path <- paste0(folder_path, "/trmm_3b43")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_years <- sub(".*3B43.", "", file_name) %>% substr(1, 4) %>% unique()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  for (dummie_layers in dummie_years) {
    dummie_list <- grep(dummie_layers, file_name, value = TRUE)
    precip <- parLapply(cluster, dummie_list, function(year){
      layer_name <- sub(".*3B43.", "", year)
      layer_name <- substr(layer_name, 1, 8)
      dummie_table <- gdalUtils::get_subdatasets(year)
      dummie_table <- rgdal::readGDAL(dummie_table[1])
      dummie_table <- raster::brick(dummie_table)
      dummie_table <- raster::t(dummie_table)
      sp::proj4string(dummie_table) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
      raster::extent(dummie_table) <- c(-180, 180, -50, 50)
      dummie_table <- raster::flip(dummie_table, direction = "y")
      dummie_table[dummie_table < 0] <- NA
      dummie_table <- raster::aggregate(dummie_table, fact = 2, fun = mean, na.rm = TRUE)
      names(dummie_table) <- layer_name
      dummie_table <- raster::as.data.frame(dummie_table, xy = TRUE, long = TRUE, na.rm = TRUE)
      data.table::setnames(dummie_table, "layer", "Z")
      dummie_table$Z <- as.Date(dummie_table$Z, format = "X%Y%m%d")
      dummie_table$value <- dummie_table$value * lubridate::days_in_month(dummie_table$Z) * 24
      return(dummie_table)
    })
    precip <- data.table::rbindlist(precip)
    precip$name <- "trmm_3b43"
    class(precip) <- append(class(precip),"pRecipe")
    saveRDS(precip, paste0(folder_path, "/../../database/trmm_3b43_", dummie_layers, ".Rds"))
  }
  stopCluster(cluster)
}

#' UDEL data reformatter
#'
#' Function for reading UDEL NC file, and reformatting them into data.table which is stored in an .Rds file.
#'
#' @param folder_path a character string with the path to the "raw" folder.
#' @export

reformat_udel <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!grepl("*/data/raw", folder_path)){
    stop("Error: folder_path should point to the location of 'data/raw'")
  }
  folder_path <- paste0(folder_path, "/udel")
  file_name <- list.files(folder_path, full.names = TRUE)
  dummie_raster <- raster::brick(file_name)
  dummie_years <- names(dummie_raster) %>% as.Date(format = "X%Y.%m.%d") %>% year() %>% unique()
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  for (dummie_layers in dummie_years) {
    dummie_list <- raster::subset(dummie_raster, grep(dummie_layers, names(dummie_raster), value = TRUE)) %>% raster::as.list()
    precip <- parLapply(cluster, dummie_list, function(year){
      year <- raster::setZ(year, as.Date(names(year), format = "X%Y.%m.%d"))
      dummie_table <- raster::as.data.frame(year, xy = TRUE, long = TRUE, na.rm = TRUE)
      dummie_table <- data.table::as.data.table(dummie_table)
      dummie_table$Z <- as.Date(dummie_table$Z)
      dummie_table$value <- dummie_table$value * 10
      return(dummie_table)
    })
    precip <- data.table::rbindlist(precip)
    precip[x > 180, x := x - 360]
    precip$name <- "udel"
    class(precip) <- append(class(precip),"pRecipe")
    saveRDS(precip, paste0(folder_path, "/../../database/udel_", dummie_layers, ".Rds"))
  }
  stopCluster(cluster)
}

#' All data reformatter
#'
#' Function for reformatting all of the available data sets.
#'
#' @param folder_path a character string with the path to the "raw" folder.
#' @export

reformat_all <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!grepl("*/data/raw", folder_path)){
    stop("Error: folder_path should point to the location of 'data/raw'")
  }
  reformat_20cr(folder_path)
  reformat_cmap(folder_path)
  reformat_cpc(folder_path)
  reformat_cru_ts(folder_path)
  reformat_ghcn(folder_path)
  reformat_gpcc(folder_path)
  reformat_gpcp(folder_path)
  reformat_gpm_imergm(folder_path)
  reformat_ncep_doe(folder_path)
  reformat_ncep_ncar(folder_path)
  reformat_precl(folder_path)
  reformat_trmm_3b43(folder_path)
  reformat_udel(folder_path)
}