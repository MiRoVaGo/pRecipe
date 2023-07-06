#' Ensure getZ output are dates
#'
#' Function to fix dates
#'
#' @importFrom methods as is
#' @param x output from getZ()
#' @return Date
#' @keywords internal

aux_date <- function(x) {
  if (is.character(x) | is.numeric(x)) {
    if (is.numeric(x)) {
      x <- as.character(x)
    }
    if (length(x[x == "00"]) >= 1) {
      x <- sub("^00$", "", x)
      x <- x[x != ""]
      x <- as.Date(x)
    } else if (!Reduce("|",grepl("-01", x))) {
      x <- as.numeric(x)
      dummie_origin <- "1970-01-01 00:00:00"
      x <- as.Date(x, origin = dummie_origin)
    } else {
      x <- as.Date(x)
    }
  }
  return(x)
}
