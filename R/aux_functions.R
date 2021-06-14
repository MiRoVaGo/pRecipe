#' Display available data sets
#'
#' Function for displaying available data sets in pRecipe.
#' @return a data.frame listing the data sets pRecipe is able to work with.
#' @export

display_data <- function(){
  dummie_table <- data.frame(name = c("20cr", "cmap", "cpc", "cru_ts", "ghcn", "gpcc", "gpcp", "gpm_imergm", "ncep_ncar", "ncep_doe", "precl", "trmm_3b43", "udel"),
                             time_res = c("Monthly", "Monthly", "Daily", "Monthly", "Monthly", "Monthly", "Monthly", "Monthly", "Monthly", "Monthly", "Monthly", "Monthly", "Monthly"),
                             time_coverage = c("1836-2015", "1979-2020", "1979-2020", "1901-2020", "1900-2015", "1891-2016", "1979-2020", "2001-2020", "1948-2020", "1979-2020", "1948-2012", "1998-2019", "1900-2017"),
                             res = c("1.0 x 1.0 degrees", "2.5 x 2.5 degrees", "0.5 x 0.5 degrees", "0.5 x 0.5 degrees", "5 x 5 degrees", "0.5 x 0.5 degrees", "2.5 x 2.5 degrees", "0.1 x 0.1 degrees", "T62 Gaussian grid", "T62 Gaussian grid", "0.5 x 0.5 degrees", "0.25 x 0.25 degrees", "0.5 x 0.5 degrees"),
                             format = c(".nc", ".nc", ".nc", ".gz", ".nc", ".nc", ".nc", ".HDF5", ".nc", ".nc", ".nc", ".HDF", ".nc"),
                             approx_size = c("392.7 MB", "14.7 MB","2.57 GB", "224.4 MB", "14.7 MB", "402.7 MB", "17.5 MB", "13.18 GB", "49.3 MB", "22.1 MB", "218.2 MB", "1.05 GB", "318.8 MB"))
  View(dummie_table, "pRecipe data sets")
}

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
#' @return No return value, called to create tidy directories for pRecipe.
#' @export

create_folders <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  dir.create(paste0(destination, "/data"))
  dir.create(paste0(destination, "/data/database"))
  dir.create(paste0(destination, "/data/integration"))
  dir.create(paste0(destination, "/data/integration/aux"))
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
#' @return a data table with the weighted average of the original.

dt_parallel <- function(dummie_table){
  dummie_table[, means := mean(value, na.rm = TRUE), by = .(x, y, month(Z))]
  dummie_table[, err := (means - value)^(-2)]
  dummie_table[, sum_err := sum(err, na.rm = TRUE), by = .(x, y, month(Z))]
  dummie_table[, weight := err/sum_err]
  dummie_table[, wvalue := value * weight]
  dummie_table <- dummie_table[, .(x, y, Z, sum_err, wvalue)]
  dummie_table[, wvalue := sum(wvalue, na.rm = TRUE), by = .(x, y, month(Z))]
  dummie_table[, sum_err := 1/sum_err]
  dummie_table <- unique(dummie_table)
  return(dummie_table)
}

#' Download and reformat ensemble spread of 20CR v3 for 1836-1890
#'
#' Function for downloading and reformatting 20cr sd values between 1836 and 1890.
#'
#' @param destination a character string with the path to the "integration/aux" folder.
#' @return No return value, called for side effects.

sd_20cr <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  file_url <- "ftp://ftp2.psl.noaa.gov/Datasets/20thC_ReanV3/spreads/Monthlies/accumsSI-MO/apcp.mon.mean.nc"
  file_destination <- paste0(destination, "/apcp.mon.sd.nc")
  download.file(file_url, file_destination, mode = "wb")
  dummie_raster <- raster::brick(file_destination)
  dummie_years <- names(dummie_raster) %>% as.Date(format = "X%Y.%m.%d") %>% year() %>% unique()
  dummie_years <- dummie_years[dummie_years <= 1890]
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
    precip$name <- "20cr_sd"
    class(precip) <- append(class(precip),"pRecipe")
    saveRDS(precip[(y >= -90) & (y <= 90)], paste0(destination, "/20cr_sd_", dummie_layers, ".Rds"))
  }
  stopCluster(cluster)
}