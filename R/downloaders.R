#' 20CR data downloader
#'
#' Function for downloading 20CR NC files.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.
#' @return No return value, called to download the data set.
#' @export

download_20cr <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  file_name <- "prate.mon.mean.nc"
  file_url_base <- "https://downloads.psl.noaa.gov/Datasets/20thC_ReanV3/Monthlies/sfcSI-MO/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(folder_path, "/20cr/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' CMAP data downloader
#'
#' Function for downloading CMAP NC files.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.
#' @return No return value, called to download the data set.
#' @export

download_cmap <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  file_url_base <- "https://downloads.psl.noaa.gov/Datasets/cmap/std/"
  file_name <- "precip.mon.mean.nc"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(folder_path, "/cmap/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' CPC data downloader
#'
#' Function for downloading CPC-GLOBAL NC files.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.
#' @param start_year numeric. Start year should be between 1979-2021.
#' @param end_year numeric. End year should be between 1979-2021, and should be greater or equal to start year.
#' @return No return value, called to download the data set.
#' @export

download_cpc <- function(folder_path, start_year = 1979, end_year = 2021){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!(is.numeric(start_year) & is.numeric(end_year))) stop ("start_year and end_year should be numeric.")
  if ((!any(start_year == 1979:2021)) | (!any(end_year == 1979:2021)) | !(end_year >= start_year)){
    stop("Error: start_year and end_year should be between 1979-2021, and end_year should be greater or equal to start_year")
  }
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  file_url_base <- "https://downloads.psl.noaa.gov/Datasets/cpc_global_precip/"
  file_url_list <- c() 
  for (year in start_year:end_year){
    file_name <- paste0("precip.", year, ".nc")
    file_url <- paste0(file_url_base, file_name)
    file_url_list <- c(file_url_list, file_url)
  }
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  clusterExport(cluster, "folder_path", envir = environment())
  parLapply(cluster, file_url_list, function(daily){
    file_name <- sub(".*/", "", daily)
    file_destination <- paste0(folder_path, "/cpc/", file_name)
    download.file(daily, file_destination, mode = "wb")
  })
  stopCluster(cluster)
}

#' CRU data downloader
#'
#' Function for downloading CRU_TS NC.GZ file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.
#' @return No return value, called to download the data set.
#' @export

download_cru_ts <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  file_name <- "cru_ts4.05.1901.2020.pre.dat.nc.gz"
  file_url_base <- "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/pre/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(folder_path, "/cru_ts/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' GHCN-M data downloader
#'
#' Function for downloading GHCN-M NC file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.
#' @return No return value, called to download the data set.
#' @export

download_ghcn <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  file_name <- "precip.mon.total.nc"
  file_url_base <- "https://downloads.psl.noaa.gov/Datasets/ghcngridded/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(folder_path, "/ghcn/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' GPCC data downloader
#'
#' Function for downloading GPCC NC file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.
#' @param resolution numeric. Data spatial resolution. Suitable options are:
#' \itemize{
#' \item{0.5 for 0.5 degree,}
#' \item{1 for 1 degree,}
#' \item{2.5 for 2.5 degree.}
#' }
#' @return No return value, called to download the data set.
#' @export

download_gpcc <- function(folder_path, resolution = 0.5){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!is.numeric(resolution)) stop ("resolution should be numeric.")
  if (!any(resolution == c(0.25, 0.5, 1, 2.5))){
    stop("Error: Resolution not available. Select between 0.5, 1, 2.5")
  }
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  file_name <- switch(as.character(resolution),
         "0.5" = "precip.mon.total.v2018.nc",
         "1" = "precip.mon.total.1x1.v2018.nc",
         "2.5" = "precip.mon.total.2.5x2.5.v2018.nc"
  )
  file_url_base <- "https://downloads.psl.noaa.gov/Datasets/gpcc/full_v2018/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(folder_path, "/gpcc/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' GPCP data downloader
#'
#' Function for downloading GPCP NC file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.
#' @return No return value, called to download the data set.
#' @export

download_gpcp <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  file_url_base <- "https://downloads.psl.noaa.gov/Datasets/gpcp/"
  file_name <- "precip.mon.mean.nc"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(folder_path, "/gpcp/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' GPM data downloader
#'
#' Function for downloading GPM HDF5 files.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.
#' @param start_year numeric. Start year should be between 2000-2020.
#' @param end_year numeric. End year should be between 2000-2020, and should be greater or equal to start year.
#' @note user must \href{https://urs.earthdata.nasa.gov}{Create an Earthdata account} and \href{https://disc.gsfc.nasa.gov/earthdata-login}{Link GES DISC}
#' @return No return value, called to download the data set.
#' @export

download_gpm_imergm <- function(folder_path, start_year = 2000, end_year = 2020){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!(is.numeric(start_year) & is.numeric(end_year))) stop ("start_year and end_year should be numeric.")
  if ((!any(start_year == 2000:2020)) | (!any(end_year == 2000:2020)) | !(end_year >= start_year)){
    stop("Error: start_year and end_year should be between 2000-2020, and end_year should be greater or equal to start_year")
  }
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  username <- getPass("Enter the username: ") %>% URLencode(reserved = TRUE)
  password <- getPass("Enter the password: ") %>% URLencode(reserved = TRUE)
  file_url_base <- paste0("https://gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGM.06/")
  file_url_list <- c()
  for (year in start_year:end_year){
    if (year == 2000){
      start_month <- 6
    } else {
      start_month <- 1
    }
    for (month in start_month:12){
      file_name <- paste0("3B-MO.MS.MRG.3IMERG.", year, str_pad(month, 2, pad = "0"), "01-S000000-E235959.", str_pad(month, 2, pad = "0"), ".V06B.HDF5")
      file_url <- paste0(file_url_base, year, "/", file_name)
      file_url_list <- c(file_url_list, file_url)
    }
  }
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  clusterExport(cluster, c("folder_path", "username", "password"), envir = environment())
  parLapply(cluster, file_url_list, function(daily){
    h <- curl::new_handle()
    curl::handle_setopt(
      handle=h,
      userpwd = paste0(username, ":", password),
      verbose=TRUE
    )
    file_name <- sub(".*/", "", daily)
    file_destination <- paste0(folder_path, "/gpm_imergm/", file_name)
    curl::curl_download(daily, file_destination, handle = h)
  })
  stopCluster(cluster)
}

#' NCEP/NCAR data downloader
#'
#' Function for downloading NCEP/NCAR NC files.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.
#' @return No return value, called to download the data set.
#' @export

download_ncep_ncar<- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  file_name <- "prate.sfc.mon.mean.nc"
  file_url_base <- "https://downloads.psl.noaa.gov/Datasets/ncep.reanalysis.derived/surface_gauss/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(folder_path, "/ncep_ncar/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' NCEP/DOE data downloader
#'
#' Function for downloading NCEP/DOE NC files.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.
#' @return No return value, called to download the data set.
#' @export

download_ncep_doe <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  file_name <- "prate.sfc.mon.mean.nc"
  file_url_base <- "https://downloads.psl.noaa.gov/Datasets/ncep.reanalysis2.derived/gaussian_grid/"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(folder_path, "/ncep_doe/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' PRECL data downloader
#'
#' Function for downloading PRECL NC file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.
#' @param resolution numeric. Data spatial resolution. Suitable options are:
#' \itemize{
#' \item{0.5 for 0.5 degree,}
#' \item{1 for 1 degree,}
#' \item{2.5 for 2.5 degree.}
#' }
#' @return No return value, called to download the data set.
#' @export

download_precl <- function(folder_path, resolution = 0.5){
  if (!is.character(folder_path)) stop ("folder_path should be character string.")
  if (!any(resolution == c(0.5, 1, 2.5))){
    stop("Error: Resolution not available. Select between 0.5, 1, 2.5")
  }
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
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
  file_url_base <-"https://downloads.psl.noaa.gov/Datasets/precl/"
  file_url <- paste0(file_url_base, file_folder, file_name)
  file_destination <- paste0(folder_path, "/precl/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' TRMM data downloader
#'
#' Function for downloading TRMM 3B43 HDF files.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.
#' @param start_year numeric. Start year should be between 1998-2019.
#' @param end_year numeric. End year should be between 1979-2019, and should be greater or equal to start year.
#' @note user must \href{https://urs.earthdata.nasa.gov}{Create an Earthdata account} and \href{https://disc.gsfc.nasa.gov/earthdata-login}{Link GES DISC} 
#' @return No return value, called to download the data set.
#' @export

download_trmm_3b43 <- function(folder_path, start_year = 1998, end_year = 2019){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  if (!(is.numeric(start_year) & is.numeric(end_year))) stop ("start_year and end_year should be numeric.")
  if ((!any(start_year == 1998:2019)) | (!any(end_year == 1998:2019)) | !(end_year >= start_year)){
    stop("Error: start_year and end_year should be between 1998-2019, and end_year should be greater or equal to start_year")
  }
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  username <- getPass("Enter the username: ") %>% URLencode(reserved = TRUE)
  password <- getPass("Enter the password: ") %>% URLencode(reserved = TRUE)
  file_url_base <- paste0("https://disc2.gesdisc.eosdis.nasa.gov/data/TRMM_L3/TRMM_3B43.7/")
  file_url_list <- c()
  for (year in start_year:end_year){
    for (month in 1:12){
      if ((year < 2000) | (year >= 2011) | ((year == 2010) && (month > 9))){
        file_name <- paste0("3B43.", year, str_pad(month, 2, pad = "0"), "01.7.HDF")
      } else {
        file_name <- paste0("3B43.", year, str_pad(month, 2, pad = "0"), "01.7A.HDF")
        }
      file_url <- paste0(file_url_base, year, "/", file_name)
      file_url_list <- c(file_url_list, file_url)
    }
  }
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  clusterExport(cluster, c("folder_path", "username", "password"), envir = environment())
  parLapply(cluster, file_url_list, function(daily){
    h <- curl::new_handle()
    curl::handle_setopt(
      handle=h,
      userpwd = paste0(username, ":", password),
      verbose=TRUE
    )
    file_name <- sub(".*/", "", daily)
    file_destination <- paste0(folder_path, "/trmm_3b43/", file_name)
    curl::curl_download(daily, file_destination, handle = h)
  })
  stopCluster(cluster)
}

#' UDEL data downloader
#'
#' Function for downloading UDEL NC file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.
#' @return No return value, called to download the data set.
#' @export

download_udel <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  file_url_base <- "https://downloads.psl.noaa.gov/Datasets/udel.airt.precip/"
  file_name <- "precip.mon.total.v501.nc"
  file_url <- paste0(file_url_base, file_name)
  file_destination <- paste0(folder_path, "/udel/", file_name)
  download.file(file_url, file_destination, mode = "wb")
}

#' All data downloader
#'
#' Function for downloading GPCP NC file.
#'
#' @param folder_path a character string with the path where the "raw" folder is located.
#' @return No return value, called to download the data set.
#' @export

download_all <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  download_20cr(folder_path)
  download_cmap(folder_path)
  download_cpc(folder_path)
  download_cru_ts(folder_path)
  download_ghcn(folder_path)
  download_gpcc(folder_path)
  download_gpcp(folder_path)
  download_gpm_imergm(folder_path)
  download_ncep_ncar(folder_path)
  download_ncep_doe(folder_path)
  download_precl(folder_path)
  download_trmm_3b43(folder_path)
  download_udel(folder_path)
}