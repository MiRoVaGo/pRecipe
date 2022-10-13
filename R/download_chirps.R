#' CHIRPS data downloader
#'
#' Function for downloading CHIRPS.
#'
#' @importFrom utils download.file
#' @param folder_path a character string with the path where the data will be downloaded.
#' @return No return value, called to download the data set.
#' @keywords internal

download_chirps <- function(folder_path = "."){
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  zenodo_base <- "https://zenodo.org/record/7094293/files/"
  zenodo_end <- "?download=1"
  file_name <- "chirps_tp_mm_land50s50n_198101_202207_025_monthly.nc"
  file_url <- paste0(zenodo_base, file_name, zenodo_end)
  file_destination <- paste(folder_path, file_name, sep = "/")
  download.file(file_url, file_destination, mode = "wb")
}