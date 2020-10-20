#' 20CR data downloader
#'
#' Function for downloading 20CR NC files.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @export

download_20cr <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  file_name <- "apcp.mon.mean.nc"
  file_url_base <- "ftp://ftp2.psl.noaa.gov/Datasets/20thC_ReanV3/Monthlies/accumsSI-MO/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' CMAP data downloader
#'
#' Function for downloading CMAP NC files.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @export

download_cmap <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  file_url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/cmap/std/"
  file_name <- "precip.mon.mean.nc"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' CPC data downloader
#'
#' Function for downloading CPC-GLOBAL NC files.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @param start_year numeric. Start year should be between 1979-2019.
#' @param end_year numeric. End year should be between 1979-2019, and should be greater or equal to start year.
#' @export

download_cpc <- function(destination, start_year, end_year){
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
    download.file(file_url, file_destination, mode = "wb")
  }
}

#' CRU data downloader
#'
#' Function for downloading CRU_TS NC.GZ file.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @export

download_cru <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  file_name <- "cru_ts4.04.1901.2019.pre.dat.nc.gz"
  file_url_base <- "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.04/cruts.2004151855.v4.04/pre/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' GHCN-M data downloader
#'
#' Function for downloading GHCN-M NC file.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @export

download_ghcn <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  file_name <- "precip.mon.total.nc"
  file_url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/ghcngridded/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' GPCC data downloader
#'
#' Function for downloading GPCC NC.GZ file.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @param resolution numeric. Data spatial resolution. Suitable options are:
#' \itemize{
#' \item{0.25 for 0.25 degree,}
#' \item{0.5 for 0.5 degree,}
#' \item{1 for 1 degree,}
#' \item{2.5 for 2.5 degree.}
#' }
#' @export



download_gpcc <- function(destination, resolution){
  if (!is.character(destination)) stop ("destination should be a character string.")
  if (!is.numeric(resolution)) stop ("resolution should be numeric.")
  if (!any(resolution == c(0.25, 0.5, 1, 2.5))){
    stop("Error: Resolution not available. Select between 0.25, 0.5, 1, 2.5")
  }
  file_name <- switch(as.character(resolution),
         "0.25" = "full_data_monthly_v2018_025.nc.gz",
         "0.5" = "full_data_monthly_v2018_05.nc.gz",
         "1" = "full_data_monthly_v2018_10.nc.gz",
         "2.5" = "full_data_monthly_v2018_25.nc.gz"
  )
  file_url_base <- "https://opendata.dwd.de/climate_environment/GPCC/full_data_2018/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' GPCP data downloader
#'
#' Function for downloading GPCP NC file.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @export

download_gpcp <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  file_url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/gpcp/"
  file_name <- "precip.mon.mean.nc"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' GPM data downloader
#'
#' Function for downloading GPM HDF5 files.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @param start_year numeric. Start year should be between 2000-2019.
#' @param end_year numeric. End year should be between 2000-2019, and should be greater or equal to start year.
#' @note user must \href{"https://wiki.earthdata.nasa.gov/display/EL/How+To+Register+For+an+EarthData+Login+Profile"}{Create an Earthdata account} and \href{https://disc.gsfc.nasa.gov/earthdata-login}{Link GES DISC}
#' @export


download_gpm <- function(destination, start_year, end_year){
  if (!is.character(destination)) stop ("destination should be a character string.")
  if (!(is.numeric(start_year) & is.numeric(end_year))) stop ("start_year and end_year should be numeric.")
  if ((!any(start_year == 2000:2019)) | (!any(end_year == 2000:2019)) | !(end_year >= start_year)){
    stop("Error: start_year and end_year should be between 2000-2019, and end_year should be greater or equal to start_year")
  }
  username <- getPass("Enter the username: ")
  password <- getPass("Enter the password: ")
  file_url_base <- paste0("https://", username, ":", password, "@", "gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGM.06/")
  for (year in start_year:end_year){
    if (year == 2000){
      start_month <- 6
    } else {
      start_month <- 1
    }
    for (month in start_month:12){
      file_name <- paste0("3B-MO.MS.MRG.3IMERG.", year, str_pad(month, 2, pad = "0"), "01-S000000-E235959.", str_pad(month, 2, pad = "0"), ".V06B.HDF5")
      file_url <- paste0(file_url_base, year, "/", file_name)
      file_destination <- paste0(destination, "/", file_name)
      download.file(file_url, file_destination, mode = "wb")
    }
  }
}

#' NCEP/NCAR data downloader
#'
#' Function for downloading NCEP/NCAR NC files.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @export

download_ncep <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  file_name <- "prate.sfc.mon.mean.nc"
  file_url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.derived/surface_gauss/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' NCEP/DOE data downloader
#'
#' Function for downloading NCEP/DOE NC files.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @export

download_ncep2 <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  file_name <- "prate.sfc.mon.mean.nc"
  file_url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2.derived/gaussian_grid/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' PRECL data downloader
#'
#' Function for downloading PRECL NC file.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @param resolution numeric. Data spatial resolution. Suitable options are:
#' \itemize{
#' \item{0.5 for 0.5 degree,}
#' \item{1 for 1 degree,}
#' \item{2.5 for 2.5 degree.}
#' }
#' @export

download_precl <- function(destination, resolution){
  if (!is.character(destination)) stop ("destination should be character string.")
  if (!any(resolution == c(0.5, 1, 2.5))){
    stop("Error: Resolution not available. Select between 0.5, 1, 2.5")
  }
  file_name <- switch(as.character(resolution),
                      "0.5" = "precip.mon.mean.0.5x0.5.nc",
                      "1" = "precip.mon.mean.1x1.nc",
                      "2.5" = "precip.mon.mean.2.5x2.5.nc"
  )
  file_folder <- switch(as.character(resolution),
                        "0.5" = "0.5deg/",
                        "1" = "1.0deg/",
                        "2.5" = "2.5deg/"
  )
  file_url_base <-"ftp://ftp.cdc.noaa.gov/Datasets/precl/"
  file_url <- paste0(file_url_base, file_folder, file_name)
  file_destination <- paste0(destination, "/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' TRMM data downloader
#'
#' Function for downloading TRMM 3B43 HDF files.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @param start_year numeric. Start year should be between 1998-2019.
#' @param end_year numeric. End year should be between 1979-2019, and should be greater or equal to start year.
#' @note user must \href{"https://wiki.earthdata.nasa.gov/display/EL/How+To+Register+For+an+EarthData+Login+Profile"}{Create an Earthdata account} and \href{https://disc.gsfc.nasa.gov/earthdata-login}{Link GES DISC} 
#' @export

download_trmm <- function(destination, start_year, end_year){
  if (!is.character(destination)) stop ("destination should be a character string.")
  if (!(is.numeric(start_year) & is.numeric(end_year))) stop ("start_year and end_year should be numeric.")
  if ((!any(start_year == 1998:2019)) | (!any(end_year == 1998:2019)) | !(end_year >= start_year)){
    stop("Error: start_year and end_year should be between 1998-2019, and end_year should be greater or equal to start_year")
  }
  username <- getPass("Enter the username: ")
  password <- getPass("Enter the password: ")
  file_url_base <- paste0("https://", username, ":", password, "@", "disc2.gesdisc.eosdis.nasa.gov/data/TRMM_L3/TRMM_3B43.7/")
  for (year in start_year:end_year){
    for (month in 1:12){
      if ((year < 2000) | (year >= 2011) | ((year == 2010) && (month > 9))){
        file_name <- paste0("3B43.", year, str_pad(month, 2, pad = "0"), "01.7.HDF")
      } else {
        file_name <- paste0("3B43.", year, str_pad(month, 2, pad = "0"), "01.7A.HDF")
        }
      file_url <- paste0(file_url_base, year, "/", file_name)
      file_destination <- paste0(destination, "/", file_name)
      download.file(file_url, file_destination, mode = "wb")
    }
  }
}

#' UDEL data downloader
#'
#' Function for downloading UDEL NC file.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @export

download_udel <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  file_url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/udel.airt.precip/"
  file_name <- "precip.mon.total.v501.nc"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}