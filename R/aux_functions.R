#' Directory creator
#'
#' Function for creating tidy data directories.
#'
#' @param destination a character string with the path where the folders will be located.

create_folders <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  dir.create(paste0(destination, "/data"))
  dir.create(paste0(destination, "/data/database"))
  dir.create(paste0(destination, "/data/integration"))
  dir.create(paste0(destination, "/data/integration/1836_1890"))
  dir.create(paste0(destination, "/data/integration/1891_1899"))
  dir.create(paste0(destination, "/data/integration/1900"))
  dir.create(paste0(destination, "/data/integration/1901_1947"))
  dir.create(paste0(destination, "/data/integration/1948_1978"))
  dir.create(paste0(destination, "/data/integration/1979_1997"))
  dir.create(paste0(destination, "/data/integration/1998_2000"))
  dir.create(paste0(destination, "/data/integration/2001_2011"))
  dir.create(paste0(destination, "/data/integration/2012_2015"))
  dir.create(paste0(destination, "/data/integration/2016"))
  dir.create(paste0(destination, "/data/integration/2017"))
  dir.create(paste0(destination, "/data/integration/2018_2019"))
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
#' @param res numeric. The upscale target resolution.
#' @return the aggregated data table at the new spatial resolution.

dt_aggregate <- function(data, res){
  dummie_name <- data$name[1]
  data <- data[,-5]
  data <- dcast(data, x + y ~ Z)
  data <- raster::rasterFromXYZ(data, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
  dummie_factor <- res/raster::res(data)
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
  dummie_table[, mean := mean(value, na.rm = TRUE), by = .(x, y, Z)]
  dummie_table[, err := (mean - value)^(-2)]
  dummie_table[, sum_err := sum(err, na.rm = TRUE), by = .(x, y, Z)]
  dummie_table[, weight := err/sum_err]
  dummie_table[, wvalue := value * weight]
  dummie_table <- dummie_table[, .(x, y, Z, sum_err, wvalue)]
  dummie_table[, wvalue := sum(wvalue, na.rm = TRUE), by = .(x, y, Z)]
  dummie_table[, sum_err := 1/sum_err]
  dummie_table <- unique(dummie_table)
  return(dummie_table)
}