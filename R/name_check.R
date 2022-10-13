#' Name checker
#'
#' Function to check if the data file follows pRecipe naming conventions.
#'
#' @param data_name a character string.
#' @return No return value, called to download the data set.
#' @keywords internal

name_check <- function(data_name){
  data_name <- sub(".*/(.+).nc.*", "\\1", data_name)
  data_name <- unlist(strsplit(data_name, "_", fixed = TRUE))
  dummie_list <- list("name" = data_name, "length" = length(data_name))
  return(dummie_list)
}