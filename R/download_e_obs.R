#' E-OBS data downloader
#'
#' Function for downloading E-OBS.
#'
#' @importFrom utils download.file
#' @param folder_path a character string with the path where the data will be downloaded.
#' @param time_res a character string with the desired time resolution. Suitable options are:
#' \itemize{
#' \item{"monthly",}
#' \item{"yearly".}
#' }
#' @return No return value, called to download the data set.
#' @export

download_e_obs <- function(folder_path = ".", time_res = "monthly"){
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  zenodo_base <- "https://zenodo.org/record/7808922/files/"
  zenodo_end <- "?download=1"
  file_name <- paste0("e-obs_tp_mm_europe_192001_202112_025_", time_res, ".nc")
  file_url <- paste0(zenodo_base, file_name, zenodo_end)
  file_destination <- paste(folder_path, file_name, sep = "/")
  try(download.file(file_url, file_destination, mode = "wb"), silent = TRUE)
}
