#' All data downloader
#'
#' Function for downloading GPCP NC file.
#'
#' @importFrom methods is
#' @param folder_path a character string with the path where the "database" folder is located.
#' @return No return value, called to download the data set.
#' @keywords internal

download_all <- function(folder_path = "./data/database/"){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  download_20cr(folder_path)
  download_chirps(folder_path)
  download_cmap(folder_path)
  download_cmorph(folder_path)
  download_cpc(folder_path)
  download_cru_ts(folder_path)
  download_em_earth(folder_path)
  download_era20c(folder_path)
  download_era5(folder_path)
  download_ghcn(folder_path)
  download_gldas_clsm(folder_path)
  download_gldas_noah(folder_path)
  download_gldas_vic(folder_path)
  download_gpcc(folder_path)
  download_gpcp(folder_path)
  download_gpm_imerg(folder_path)
  download_mswep(folder_path)
  download_ncep_doe(folder_path)
  download_ncep_ncar(folder_path)
  download_persiann(folder_path)
  download_precl(folder_path)
  download_terraclimate(folder_path)
  download_trmm_3b43(folder_path)
  download_udel(folder_path)
}