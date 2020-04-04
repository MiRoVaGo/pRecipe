download_cpc_global <- function(destination){
  if (!is.character(destination)) stop ("destination should be character string.")
  file_url_base <- "https://ftp.cpc.ncep.noaa.gov/precip/CPC_UNI_PRCP/GAUGE_GLB/V1.0/"
  destination_base <- destination
  for (year in 1979:2005){
    start_date <- as.Date(paste0(year, "/1/1"))
    end_date <- as.Date(paste0(year, "/12/31"))
    days_of_year <- seq(start_date, end_date, "days")
    for (index in 1:length(days_of_year)){
      dates <- format(days_of_year[index], "%Y%m%d")
      file_name <- paste0("PRCP_CU_GAUGE_V1.0GLB_0.50deg.lnx.", dates, ".gz")
      file_url <- paste0(file_url_base, year, "/", file_name)
      destination <- paste0(destination_base, "/", year, "/", file_name)
      download.file(file_url,destination)
    }
  }
  file_url_base <- "http://ftp.cpc.ncep.noaa.gov/precip/CPC_UNI_PRCP/GAUGE_GLB/RT/"
  for (year in 2006:2019){
    start_date <- as.Date(paste0(year, "/1/1"))
    end_date <- as.Date(paste0(year, "/12/31"))
    days_of_year <- seq(start_date, end_date, "days")
    for (index in 1:length(days_of_year)){
      dates <- format(days_of_year[index], "%Y%m%d")
      file_name <- paste0("PRCP_CU_GAUGE_V1.0GLB_0.50deg.lnx.", dates, "RT.gz")
      file_url <- paste0(file_url_base, year, "/", file_name)
      destination <- paste0(destination_base, "/", year, "/", file_name)
      download.file(file_url,destination)
    }
  }
}