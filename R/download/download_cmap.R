download_cmap <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  file_url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/cmap/enh/"
  file_name <- "precip.mon.mean.nc"
  file_url <- paste0(file_url_base, file_name)
  destination <- paste0(destination, "/", file_name)
  download.file(file_url,destination)
}