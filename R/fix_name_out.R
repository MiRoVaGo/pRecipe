#' Fix name out
#'
#' Function to fix the names when subsetting 
#'
#' @importFrom methods as
#' @param nc_out a character string
#' @return No return value, called to rename subsetted file
#' @keywords internal

fix_name_out <- function(nc_out){
  dummie_name <- name_check(nc_out)
  dummie_date <- show_info(nc_out)
  dummie_date <- dummie_date[8]
  dummie_date <- unlist(strsplit(dummie_date, " ", fixed = TRUE))
  dummie_date <- grep("-", dummie_date, value = TRUE)
  dummie_date <- substr(dummie_date, 1, 7)
  dummie_date <- sub("-", "", dummie_date)
  if (dummie_name$length == 8) {
    dummie_name$name[5] <- dummie_date[1]
    dummie_name$name[6] <- dummie_date[2]
    dummie_name <- paste(dummie_name$name, collapse = "_")
    dummie_name <- paste0(dummie_name, ".nc")
    nc_mid <- sub("(.*/)(.*)", "\\1", nc_out)
    dummie_name <- paste0(nc_mid, dummie_name)
    dummie_name <- sub(".nc.nc.*", ".nc", dummie_name)
    if (!file.exists(dummie_name)) {
      file.rename(nc_out, dummie_name)
    } else {
      warning("Couldn't fix the filename to pRecipe convention\nBecause a file with the same name already existst")
    }
  }
}