download_gpcc_daily <- function(destination){
  if (!is.character(destination)) stop ("destination should be character string.")
  file_url_base <- "https://opendata.dwd.de/climate_environment/GPCC/full_data_daily_V2018/"
  destination_base <- destination
  for (year in 1982:2016){
    file_name <- paste0("full_data_daily_v2018_", year, ".nc.gz")
    file_url <- paste0(file_url_base, file_name)
    destination <- paste0(destination_base, "/", file_name)
    download.file(file_url,destination)
  }
}