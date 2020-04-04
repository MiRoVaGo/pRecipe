download_ghcn <- function(destination){
  if (!is.character(destination)) stop ("destination should be character string.")
  file_url <- "ftp://ftp.cdc.noaa.gov/Datasets/ghcngridded/precip.mon.total.nc"
  destination <- paste0(destination, "/precip.mon.total.nc")
  download.file(file_url,destination)
}