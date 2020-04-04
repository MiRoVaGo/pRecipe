download_persiann <- function(destination){
  if (!is.character(destination)) stop ("destination should be character string.")
  file_url_base <- "ftp://persiann.eng.uci.edu/CHRSdata/PERSIANN/mthly/"
  destination_base <- destination
  start_date <- as.Date("2000/3/1")
  end_date <- as.Date("2019/12/31")
  days_of_year <- seq(start_date, end_date, "months")
  for (index in 1:length(days_of_year)){
    dates <- format(days_of_year[index], "%y%m")
    file_name <- paste0("ms6s4_m", dates, ".bin.gz")
    file_url <- paste0(file_url_base, file_name)
    destination <- paste0(destination_base, "/", file_name)
    download.file(file_url,destination)
  }
}
