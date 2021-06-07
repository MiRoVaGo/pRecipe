#' 20CR data downloader
#'
#' Function for downloading 20CR NC files.
#'
#' @param destination a character string with the path where the downloaded file is saved.

download_20cr <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  file_name <- "apcp.mon.mean.nc"
  file_url_base <- "ftp://ftp2.psl.noaa.gov/Datasets/20thC_ReanV3/Monthlies/accumsSI-MO/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/20cr/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' CMAP data downloader
#'
#' Function for downloading CMAP NC files.
#'
#' @param destination a character string with the path where the downloaded file is saved.

download_cmap <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  file_url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/cmap/std/"
  file_name <- "precip.mon.mean.nc"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/cmap/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' CMORPH data downloader
#'
#' Function for downloading CMORPH NC files.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @param start_year numeric. Start year should be between 1998-2020.
#' @param end_year numeric. End year should be between 1998-2020, and should be greater or equal to start year.

download_cmorph <- function(destination, start_year = 1998, end_year = 2020){
  if (!is.character(destination)) stop ("destination should be a character string.")
  if (!(is.numeric(start_year) & is.numeric(end_year))) stop ("start_year and end_year should be numeric.")
  if ((!any(start_year == 1998:2020)) | (!any(end_year == 1998:2020)) | !(end_year >= start_year)){
    stop("Error: start_year and end_year should be between 1998-2020, and end_year should be greater or equal to start_year")
  }
  file_url_base <- "https://www.ncei.noaa.gov/data/cmorph-high-resolution-global-precipitation-estimates/access/daily/0.25deg/"
  for (year in start_year:end_year){
    for(month in 1:12){
      for (day in 1:days_in_month(month)){
        file_name <- paste0("CMORPH_V1.0_ADJ_0.25deg-DLY_00Z_", year, str_pad(month, 2, pad = "0"), str_pad(day, 2, pad = "0"), ".nc")
        file_url <- paste0(file_url_base, year, "/", str_pad(month, 2, pad = "0"), "/", file_name)
        file_destination <- paste0(destination, "/cmorph/", file_name)
        download.file(file_url, file_destination, mode = "wb")
      }
    }
  }
}

#' CPC data downloader
#'
#' Function for downloading CPC-GLOBAL NC files.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @param start_year numeric. Start year should be between 1979-2020.
#' @param end_year numeric. End year should be between 1979-2020, and should be greater or equal to start year.

download_cpc <- function(destination, start_year = 1979, end_year = 2020){
  if (!is.character(destination)) stop ("destination should be a character string.")
  if (!(is.numeric(start_year) & is.numeric(end_year))) stop ("start_year and end_year should be numeric.")
  if ((!any(start_year == 1979:2020)) | (!any(end_year == 1979:2020)) | !(end_year >= start_year)){
    stop("Error: start_year and end_year should be between 1979-2020, and end_year should be greater or equal to start_year")
  }
  
  file_url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip/"
  for (year in start_year:end_year){
    file_name <- paste0("precip.", year, ".nc")
    file_url <- paste0(file_url_base, file_name)
    file_destination <- paste0(destination, "/cpc/", file_name)
    download.file(file_url, file_destination, mode = "wb")
  }
}

#' CRU data downloader
#'
#' Function for downloading CRU_TS NC.GZ file.
#'
#' @param destination a character string with the path where the downloaded file is saved.

download_cru_ts <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  file_name <- "cru_ts4.05.1901.2020.pre.dat.nc.gz"
  file_url_base <- "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/pre/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/cru_ts/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' GHCN-M data downloader
#'
#' Function for downloading GHCN-M NC file.
#'
#' @param destination a character string with the path where the downloaded file is saved.

download_ghcn <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  file_name <- "precip.mon.total.nc"
  file_url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/ghcngridded/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/ghcn/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' GPCC data downloader
#'
#' Function for downloading GPCC NC file.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @param resolution numeric. Data spatial resolution. Suitable options are:
#' \itemize{
#' \item{0.5 for 0.5 degree,}
#' \item{1 for 1 degree,}
#' \item{2.5 for 2.5 degree.}
#' }

download_gpcc <- function(destination, resolution = 0.5){
  if (!is.character(destination)) stop ("destination should be a character string.")
  if (!is.numeric(resolution)) stop ("resolution should be numeric.")
  if (!any(resolution == c(0.25, 0.5, 1, 2.5))){
    stop("Error: Resolution not available. Select between 0.5, 1, 2.5")
  }
  file_name <- switch(as.character(resolution),
         "0.5" = "precip.mon.total.v2018.nc",
         "1" = "precip.mon.total.1x1.v2018.nc",
         "2.5" = "precip.mon.total.2.5x2.5.v2018.nc"
  )
  file_url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/gpcc/full_v2018/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/gpcc/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' GPCP data downloader
#'
#' Function for downloading GPCP NC file.
#'
#' @param destination a character string with the path where the downloaded file is saved.

download_gpcp <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  file_url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/gpcp/"
  file_name <- "precip.mon.mean.nc"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/gpcp/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' GPM data downloader
#'
#' Function for downloading GPM HDF5 files.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @param start_year numeric. Start year should be between 2000-2020.
#' @param end_year numeric. End year should be between 2000-2020, and should be greater or equal to start year.
#' @note user must \href{https://urs.earthdata.nasa.gov}{Create an Earthdata account} and \href{https://disc.gsfc.nasa.gov/earthdata-login}{Link GES DISC}

download_gpm_imergm <- function(destination, start_year = 2000, end_year = 2020){
  if (!is.character(destination)) stop ("destination should be a character string.")
  if (!(is.numeric(start_year) & is.numeric(end_year))) stop ("start_year and end_year should be numeric.")
  if ((!any(start_year == 2000:2020)) | (!any(end_year == 2000:2020)) | !(end_year >= start_year)){
    stop("Error: start_year and end_year should be between 2000-2020, and end_year should be greater or equal to start_year")
  }
  username <- getPass("Enter the username: ") %>% URLencode(reserved = TRUE)
  password <- getPass("Enter the password: ") %>% URLencode(reserved = TRUE)
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
      file_destination <- paste0(destination, "/gpm_imergm/", file_name)
      download.file(file_url, file_destination, mode = "wb")
    }
  }
}

#' NCEP/NCAR data downloader
#'
#' Function for downloading NCEP/NCAR NC files.
#'
#' @param destination a character string with the path where the downloaded file is saved.

download_ncep_ncar<- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  file_name <- "prate.sfc.mon.mean.nc"
  file_url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.derived/surface_gauss/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/ncep_ncar/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' NCEP/DOE data downloader
#'
#' Function for downloading NCEP/DOE NC files.
#'
#' @param destination a character string with the path where the downloaded file is saved.


download_ncep_doe <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  file_name <- "prate.sfc.mon.mean.nc"
  file_url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2.derived/gaussian_grid/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/ncep_doe/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' PERSIANN CDR data downloader
#'
#' Function for downloading PERSIANN CDR NC files.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @param start_year numeric. Start year should be between 1983-2020.
#' @param end_year numeric. End year should be between 1983-2020, and should be greater or equal to start year.

download_persiann_cdr <- function(destination, start_year = 1983, end_year = 2020){
  if (!is.character(destination)) stop ("destination should be a character string.")
  if (!(is.numeric(start_year) & is.numeric(end_year))) stop ("start_year and end_year should be numeric.")
  if ((!any(start_year == 1983:2020)) | (!any(end_year == 1983:2020)) | !(end_year >= start_year)){
    stop("Error: start_year and end_year should be between 1983-2020, and end_year should be greater or equal to start_year")
  }
  file_url_base <- "https://www.ncei.noaa.gov/data/precipitation-persiann/access/"
  for (year in start_year:end_year){
    for(month in 1:12){
      if ((year >= 1983) && (year <= 2012)){
        last_pattern <- "_c20140523.nc"
      } else if ((year == 2013) | ((year == 2014) && (month <= 3))){
        last_pattern <- "_c20140908.nc"
      } else if ((year == 2014) && (month > 3)){
        last_pattern <- "_c20150330.nc"
      } else if ((year == 2015) && (month <= 3)){
        last_pattern <- "_c20150728.nc"
      } else if ((year == 2015) && (month > 3) && (month <= 6)){
        last_pattern <- "_c20151028.nc"
      } else if ((year == 2015) && (month > 6)){
        last_pattern <- "_c20160324.nc"
      } else if ((year == 2016) && (month <= 3)){
        last_pattern <- "_c20160720.nc"
      } else if ((year == 2016) && (month > 3) && (month <= 6)){
        last_pattern <- "_c20161115.nc"
      } else if ((year == 2016) && (month > 6) && (month <= 8)){
        last_pattern <- "_c20170119.nc"
      } else if (((year == 2016) && (month > 8)) | ((year == 2017) && (month <= 4))){
        last_pattern <- "_c20170930.nc"
      } else if ((year == 2017) && (month == 5)){
        last_pattern <- "_c20171211.nc"
      } else if ((year == 2017) && (month >= 6) && (month <= 7)){
        last_pattern <- "_c20180801.nc"
      } else if ((year == 2017) && (month > 7)){
        last_pattern <- "_c20180327.nc"
      } else if ((year == 2018) && (month <= 6)){
        last_pattern <- "_c20181102.nc"
      } else if ((year == 2018) && (month > 6) && (month <= 9)){
        last_pattern <- "_c20190204.nc"
      } else if ((year == 2018) && (month > 6)){
        last_pattern <- "_c20190509.nc"
      } else if ((year == 2019) && (month <= 3)){
        last_pattern <- "_c20190806.nc"
      } else if ((year == 2019) && (month > 3) && (month <= 6)){
        last_pattern <- "_c20190923.nc"
      } else if ((year == 2019) && (month > 6) && (month <= 9)){
        last_pattern <- "_c20200214.nc"
      } else if ((year == 2019) && (month > 9)){
        last_pattern <- "_c20200522.nc"
      } else if ((year == 2020) && (month <= 3)){
        last_pattern <- "_c20200810.nc"
      } else if ((year == 2020) && (month > 3) && (month <= 9)){
        last_pattern <- "_c20210208.nc"
      } else if ((year == 2020) && (month > 9)){
        last_pattern <- "_c20210504.nc"
      }
      for (day in 1:days_in_month(month)){
        file_name <- paste0("PERSIANN-CDR_v01r01_", year, str_pad(month, 2, pad = "0"), str_pad(day, 2, pad = "0"), last_pattern)
        file_url <- paste0(file_url_base, year, "/", file_name)
        file_destination <- paste0(destination, "/persiann_cdr/", file_name)
        download.file(file_url, file_destination, mode = "wb")
      }
    }
  }
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

download_precl <- function(destination, resolution = 0.5){
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
  file_destination <- paste0(destination, "/precl/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' TRMM data downloader
#'
#' Function for downloading TRMM 3B43 HDF files.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @param start_year numeric. Start year should be between 1998-2020.
#' @param end_year numeric. End year should be between 1979-2020, and should be greater or equal to start year.
#' @note user must \href{https://urs.earthdata.nasa.gov}{Create an Earthdata account} and \href{https://disc.gsfc.nasa.gov/earthdata-login}{Link GES DISC} 

download_trmm_3b43 <- function(destination, start_year = 1998, end_year = 2020){
  if (!is.character(destination)) stop ("destination should be a character string.")
  if (!(is.numeric(start_year) & is.numeric(end_year))) stop ("start_year and end_year should be numeric.")
  if ((!any(start_year == 1998:2020)) | (!any(end_year == 1998:2020)) | !(end_year >= start_year)){
    stop("Error: start_year and end_year should be between 1998-2020, and end_year should be greater or equal to start_year")
  }
  username <- getPass("Enter the username: ") %>% URLencode(reserved = TRUE)
  password <- getPass("Enter the password: ") %>% URLencode(reserved = TRUE)
  file_url_base <- paste0("https://", username, ":", password, "@", "disc2.gesdisc.eosdis.nasa.gov/data/TRMM_L3/TRMM_3B43.7/")
  for (year in start_year:end_year){
    for (month in 1:12){
      if ((year < 2000) | (year >= 2011) | ((year == 2010) && (month > 9))){
        file_name <- paste0("3B43.", year, str_pad(month, 2, pad = "0"), "01.7.HDF")
      } else {
        file_name <- paste0("3B43.", year, str_pad(month, 2, pad = "0"), "01.7A.HDF")
        }
      file_url <- paste0(file_url_base, year, "/", file_name)
      file_destination <- paste0(destination, "/trmm_3b43/", file_name)
      download.file(file_url, file_destination, mode = "wb")
    }
  }
}

#' UDEL data downloader
#'
#' Function for downloading UDEL NC file.
#'
#' @param destination a character string with the path where the downloaded file is saved.

download_udel <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  file_url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/udel.airt.precip/"
  file_name <- "precip.mon.total.v501.nc"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(destination, "/udel/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' All data downloader
#'
#' Function for downloading GPCP NC file.
#'
#' @param destination a character string with the path where the downloaded file is saved.

download_all <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  download_20cr(destination)
  download_cmap(destination)
  download_cpc(destination)
  download_cru_ts(destination)
  download_ghcn(destination)
  download_gpcc(destination)
  download_gpcp(destination)
  download_gpm_imergm(destination)
  download_ncep_ncar(destination)
  download_ncep_doe(destination)
  download_precl(destination)
  download_trmm_3b43(destination)
  download_udel(destination)
}