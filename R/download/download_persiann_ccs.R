download_persiann_ccs <- function(destination){
  if (!is.character(destination)) stop ("destination should be character string.")
  file_url_base <- "ftp://persiann.eng.uci.edu/pub/GCCS/mthly/"
  destination_base <- destination
  start_date <- as.Date("2003/1/1")
  end_date <- as.Date("2016/07/31")
  days_of_year <- seq(start_date, end_date, "months")
  for (index in 1:length(days_of_year)){
    dates <- format(days_of_year[index], "%y%m")
    file_name <- paste0("rgccs1m", dates, ".bin.gz")
    file_url <- paste0(file_url_base, file_name)
    destination <- paste0(destination_base, "/", file_name)
    download.file(file_url,destination)
  }
}