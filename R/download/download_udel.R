download_udel <- function(destination){
  if (!is.character(destination)) stop ("destination should be character string.")
  file_url <- "http://climate.geog.udel.edu/~climate/html_pages/Global2017/precip_2017.tar.gz"
  destination <- paste0(destination, "/precip_2017.tar.gz")
  download.file(file_url,destination)
}
