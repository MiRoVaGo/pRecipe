#' ERA20C data downloader
#'
#' Function for downloading ERA-20C.
#'
#' @importFrom utils download.file
#' @param folder_path a character string with the path where the data will be downloaded.
#' @param domain a character string with the desired domain data set. Suitable options are:
#' \itemize{
#' \item{"raw" for default available spatial coverage,}
#' \item{"global" for data sets with global (land and ocean) coverage,}
#' \item{"land" for data sets with land only coverage,}
#' \item{"ocean", for data sets with ocean only coverage.}
#' }
#' @return No return value, called to download the data set.
#' @keywords internal

download_era20c <- function(folder_path = ".", domain = "raw"){
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  if (domain == "raw"){domain <- "global"}
  zenodo_base <- "https://zenodo.org/record/7808922/files/"
  zenodo_end <- "?download=1"
  file_name <- paste0("era20c_tp_mm_", domain, "_19001_201012_025_monthly.nc")
  file_url <- paste0(zenodo_base, file_name, zenodo_end)
  file_destination <- paste(folder_path, file_name, sep = "/")
  try(download.file(file_url, file_destination, mode = "wb"), silent = TRUE)
}