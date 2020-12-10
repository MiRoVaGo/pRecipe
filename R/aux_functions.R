#' Directory creator
#'
#' Function for creating tidy data directories.
#'
#' @param destination a character string with the path where the folders will be located.

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
  dir.create(paste0(destination, "/data/shapefiles"))
}