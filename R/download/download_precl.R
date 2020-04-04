download_precl <- function(destination){
  if (!is.character(destination)) stop ("destination should be character string.")
  file_url <- "ftp://ftp.cdc.noaa.gov/Datasets/precl/0.5deg/precip.mon.mean.0.5x0.5.nc"
  destination <- paste0(destination, "/precip.mon.mean.0.5x0.5.nc")
  download.file(file_url,destination)
}