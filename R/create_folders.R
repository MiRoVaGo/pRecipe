#' Directory creator
#'
#' Function for creating tidy folders to store pRecipe.
#'
#' @importFrom methods is
#' @param destination a character string with the path where the folders will be created as follows:
#' \itemize{
#' \item{\code{destination}/data}
#' \itemize{
#' \item{\code{destination}/data/database}
#' \item{\code{destination}/data/processed}
#' }
#' }
#' @return No return value, called to create tidy directories for pRecipe.
#' @keywords internal

create_folders <- function(destination){
  if (!is.character(destination)) stop ("destination should be a character string.")
  dir.create(paste0(destination, "/data"))
  dir.create(paste0(destination, "/data/database"), showWarnings = FALSE)
  dir.create(paste0(destination, "/data/processed"), showWarnings = FALSE)
}