#' GLDAS_CLSM data downloader
#'
#' Function for downloading GLDAS CLSM.
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
#' @param time_res a character string with the desired time resolution. Suitable options are:
#' \itemize{
#' \item{"daily",}
#' \item{"monthly",}
#' \item{"yearly".}
#' }
#' @return No return value, called to download the data set.
#' @keywords internal

download_gldas_clsm <- function(folder_path = ".", domain = "raw", time_res = "monthly"){
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  if (domain == "raw" | domain == "land"){
    domain <- "land"
  } else {
    warning(paste0('The ', domain, ' domain is not available'))
  }
  zenodo_base <- "https://zenodo.org/record/14290970/files/"
  if (time_res == "daily") {zenodo_base <- "https://zenodo.org/record/14290969/files/"}
  zenodo_end <- "?download=1"
  file_name <- paste0("gldas-clsm-v2-0_tp_mm_", domain, "_194801_201412_025_", time_res, ".nc")
  file_url <- paste0(zenodo_base, file_name, zenodo_end)
  file_destination <- paste(folder_path, file_name, sep = "/")
  try(download.file(file_url, file_destination, mode = "wb"), silent = TRUE)
}