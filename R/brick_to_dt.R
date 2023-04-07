#' Transform raster into data.table
#'
#' Function to transform a raster brick into a data.table
#'
#' @import  data.table
#' @importFrom methods as
#' @importFrom raster as.data.frame
#' @importFrom R.utils getAbsolutePath
#' @param x a character string with the path to the data file.
#' @return data.table
#' @export
#' @examples
#' \dontrun{
#' x <- brick_to_dt("gpcp_tp_mm_global_197901_202205_025_monthly.nc")
#' y <- brick_to_dt("dummie.nc")
#' }

brick_to_dt <- function(x){
  nc_in <- getAbsolutePath(x)
  checker <- name_check(x)
  if (checker$length != 8) {
    warning("This is not pRecipe data")
  }
  dummie_cols <- aux_ts(checker$name[1])
  if (is.character(x)){
    dummie_brick <- brick(nc_in)
  } else {
    dummie_brick <- x
  }
  dummie_table <- as.data.frame(dummie_brick, xy = TRUE, long = TRUE,
                                na.rm = TRUE)
  dummie_table <- as.data.table(dummie_brick)
  dummie_table$name <- dummie_cols[1]
  dummie_table$type <- dummie_cols[2]
  return(dummie_table)
}