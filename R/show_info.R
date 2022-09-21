#' Show data content
#'
#' The function \code{show_info} displays the specification of the desired file.
#'
#' @importFrom raster brick
#' @importFrom utils capture.output
#' @param nc_path a character with the path to the desired file
#' @return character vector with screen print out
#' @export

show_info <- function(nc_path){
  if (is(nc_path, "RasterBrick") | is(nc_path, "RasterLayer") | is(nc_path, "RasterStack")) {
    dummie <- capture.output(nc_path)
    dummie <- dummie[-length(dummie)]
  } else {
    nc_path <- brick(nc_path)
    dummie <- capture.output(nc_path)
    dummie <- dummie[-length(dummie)]
  }
  return(dummie)
}