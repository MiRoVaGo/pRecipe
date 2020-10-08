download_cpc_global <- function(destination, start_year, end_year){
  if (!is.character(destination)) stop ("destination should be a character string.")
  if (!(is.numeric(start_year) & is.numeric(end_year))) stop ("start_year and end_year should be numeric.")
  if ((!any(start_year == 1979:2019)) | (!any(end_year == 1979:2019)) | !(end_year >= start_year)){
    stop("Error: start_year and end_year should be between 1979-2019, and end_year should be greater or equal to start_year")
  }
  
  file_url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip/"
  for (year in start_year:end_year){
    file_name <- paste0("precip.", year, ".nc")
    file_url <- paste0(file_url_base, file_name)
    file_destination <- paste0(destination, "/", file_name)
    download.file(file_url,file_destination)
  }
}