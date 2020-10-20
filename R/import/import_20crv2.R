import_20cr <- function(folder_path){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  file_name <- list.files(folder_path, full.names = TRUE)
  precip <- brick(file_name)
  precip[precip == -9.96921E36] <- NA
  precip <- precip * 86400
  return(precip)
}
