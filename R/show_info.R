#' Show data content
#'
#' The function \code{show_info} displays the specification of the desired file.
#'
#' @importFrom raster brick
#' @importFrom utils capture.output
#' @param x a character with the path to the desired file
#' @return character vector with screen print out
#' @export

show_info <- function(x){
  if (is(x, "RasterBrick") | is(x, "RasterLayer") | is(x, "RasterStack")) {
    dummie <- capture.output(x)
    dummie <- dummie[-length(dummie)]
  } else {
    x <- brick(x)
    dummie <- capture.output(x)
    dummie <- dummie[-length(dummie)]
  }
  return(dummie)
}