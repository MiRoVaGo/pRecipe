#' Directory creator
#'
#' Function for creating tidy folders to store pRecipe.
#'
#' @param destination a character string with the path where the folders will be created as follows:
#' \itemize{
#' \item{\code{destination}/data}
#' \itemize{
#' \item{\code{destination}/data/database}
#' \item{\code{destination}/data/raw}
#' \itemize{
#' \item{\code{destination}/data/raw/20cr}
#' \item{\code{destination}/data/raw/cmap}
#' \item{\code{destination}/data/raw/...}}}
#'}
#' 

create_folders <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  dir.create(paste0(destination, "/data"))
  dir.create(paste0(destination, "/data/database"))
  dir.create(paste0(destination, "/data/raw"))
  dir.create(paste0(destination, "/data/raw/20cr"))
  dir.create(paste0(destination, "/data/raw/cmap"))
  dir.create(paste0(destination, "/data/raw/cpc"))
  dir.create(paste0(destination, "/data/raw/cru_ts"))
  dir.create(paste0(destination, "/data/raw/ghcn"))
  dir.create(paste0(destination, "/data/raw/gpcc"))
  dir.create(paste0(destination, "/data/raw/gpcp"))
  dir.create(paste0(destination, "/data/raw/gpm_imergm"))
  dir.create(paste0(destination, "/data/raw/ncep_ncar"))
  dir.create(paste0(destination, "/data/raw/ncep_doe"))
  dir.create(paste0(destination, "/data/raw/precl"))
  dir.create(paste0(destination, "/data/raw/trmm_3b43"))
  dir.create(paste0(destination, "/data/raw/udel"))
}

#' Data table aggregation in space
#'
#' Function for upscaling spatial resolution on pRecipe data tables
#'
#' @param data data.table. A precipitation data table reformatted by pRecipe.
#' @param resolution numeric. The upscale target resolution.
#' @return the aggregated data table at the new spatial resolution.

dt_aggregate <- function(data, resolution){
  dummie_name <- data$name[1]
  data <- data[,-5]
  data <- dcast(data, x + y ~ Z)
  data <- raster::rasterFromXYZ(data, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
  dummie_factor <- resolution/raster::res(data)
  data <- raster::aggregate(data, fact = dummie_factor, fun = sum, na.rm = TRUE)
  data <- raster::as.data.frame(data, xy = TRUE, long = TRUE, na.rm = TRUE)
  data <- data.table::as.data.table(data)
  data$layer <- as.Date(data$layer, format = "X%Y.%m.%d")
  data.table::setnames(data, "layer", "Z")
  data$name <- dummie_name
  return(data)
}

#' Data table weighted mean for parallel computing
#'
#' Function for merging data sets available in pRecipe data table by weighted mean.
#'
#' @param dummie_table A pRecipe data table with multiple data sets.
#' @return the aggregated data table at the new spatial resolution.

dt_parallel <- function(dummie_table){
  dummie_table[, Z := zoo::as.yearmon(Z)]
  dummie_table[, means := mean(value, na.rm = TRUE), by = .(x, y, Z)]
  dummie_table[, err := (means - value)^(-2)]
  dummie_table[, sum_err := sum(err, na.rm = TRUE), by = .(x, y, Z)]
  dummie_table[, weight := err/sum_err]
  dummie_table[, wvalue := value * weight]
  dummie_table <- dummie_table[, .(x, y, Z, sum_err, wvalue)]
  dummie_table[, wvalue := sum(wvalue, na.rm = TRUE), by = .(x, y, Z)]
  dummie_table[, sum_err := 1/sum_err]
  dummie_table <- unique(dummie_table)
  return(dummie_table)
}

#' Download and reformat ensemble spread of 20CR v3 for 1836-1890
#'
#' Function for downloading and reformatting 20cr sd values between 1836 and 1890.
#'
#' @param destination a character string with the path to the "integration/1838_1890" folder.

sd_20cr <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  options(timeout = 600)
  file_url <- "ftp://ftp2.psl.noaa.gov/Datasets/20thC_ReanV3/spreads/Monthlies/accumsSI-MO/apcp.mon.mean.nc"
  file_destination <- paste0(destination, "/apcp.mon.sd.nc")
  download.file(file_url, file_destination, mode = "wb")
  dummie_list <- paste0(destination, "/apcp.mon.sd.nc") %>% raster::brick() %>% raster::as.list()
  dummie_list <- dummie_list[1:660]
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  precip <- parLapply(cluster, dummie_list, function(year){
    dummie_table <- raster::disaggregate(year, fact = raster::res(year)/0.5)
    dummie_table[dummie_table < 0] <- NA
    dummie_table <- raster::as.data.frame(dummie_table, xy = TRUE, long = TRUE, na.rm = TRUE)
    dummie_table <- data.table::as.data.table(dummie_table)
    dummie_table$layer <- as.Date(dummie_table$layer, format = "X%Y.%m.%d")
    data.table::setnames(dummie_table, "layer", "Z")
    dummie_table$value <- dummie_table$value
    return(dummie_table)
  })
  stopCluster(cluster)
  rm(dummie_list)
  precip <- data.table::rbindlist(precip)
  precip <- precip[x > 180, x := x - 360]
  class(precip) <- append(class(precip),"pRecipe")
  saveRDS(precip, paste0(destination, "/20cr_sd.Rds"))
}