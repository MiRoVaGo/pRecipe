download_20crv3 <- function(destination){
  if (!is.character(destination)) stop ("destination should be character string.")
  file_url_base <- "https://www.esrl.noaa.gov/psd/thredds/catalog/Datasets/20thC_ReanV3/sfcMO/catalog.html?dataset=Datasets/20thC_ReanV3/sfcMO/"
  destination_base <- destination
  for (year in 1981:2015){
    file_name <- paste0("prate.", year, ".nc")
    file_url <- paste0(file_url_base, file_name)
    destination <- paste0(destination_base, "/", file_name)
    download.file(file_url,destination)
  }
}