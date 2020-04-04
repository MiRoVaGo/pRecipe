download_ncep2 <- function(destination){
  if (!is.character(destination)) stop ("destination should be character string.")
  file_url_base <- "https://www.esrl.noaa.gov/psd/thredds/catalog/Datasets/ncep.reanalysis2/gaussian_grid/catalog.html?dataset=Datasets/ncep.reanalysis2/gaussian_grid/"
  destination_base <- destination
  for (year in 1979:2019){
    file_name <- paste0("prate.sfc.gauss.", year, ".nc")
    file_url <- paste0(file_url_base, file_name)
    destination <- paste0(destination_base, "/", file_name)
    download.file(file_url,destination)
  }
}