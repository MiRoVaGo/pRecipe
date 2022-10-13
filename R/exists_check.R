#' Existance checker
#'
#' Function to check if the data file already exists.
#'
#' @param data_name a character string.
#' @return No return value, called to download the data set.
#' @keywords internal

exists_check <- function(data_name){
  if (file.exists(data_name)){
    stop_sms <- paste0("An outputfile with the same name already exists!\nWatch out (move/rename/delete):\n", data_name)
    dummie_list <- list("exists" = file.exists(data_name), "sms" = stop_sms)
  }
  dummie_list <- list("exists" = file.exists(data_name))
  return(dummie_list)
}