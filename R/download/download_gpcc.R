download_gpcc <- function(destination){
  if (!is.character(destination)) stop ("destination should be character string.")
  file_url_base <- "https://opendata.dwd.de/climate_environment/GPCC/full_data_2018/"
  file_name <- "full_data_monthly_v2018_05.nc.gz"
  file_url <- paste0(file_url_base, file_name)
  destination <- paste0(destination, "/", file_name)
  download.file(file_url,destination)
}