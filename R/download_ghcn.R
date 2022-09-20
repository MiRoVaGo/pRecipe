#' GHCN data downloader
#'
#' Function for downloading GHCN v2.
#'
#' @importFrom methods is
#' @importFrom utils download.file
#' @param folder_path a character string with the path where the "raw" folder is located.
#' @return No return value, called to download the data set.
#' @keywords internal

download_ghcn <- function(folder_path = "./data/raw/"){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  zenodo_base <- "https://zenodo.org/record/7094293/files/"
  zenodo_end <- "?download=1"
  file_name <- "ghcn_tp_mm_land_190001_201505_025_monthly.nc"
  file_url <- paste0(zenodo_base, file_name, zenodo_end)
  file_destination <- paste0(folder_path, file_name)
  download.file(file_url, file_destination, mode = "wb")
}