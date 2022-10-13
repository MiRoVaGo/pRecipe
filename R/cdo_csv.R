#' Format cdo generated csv file
#'
#' Function to compute the weighted average of an .nc file
#'
#' @import data.table
#' @param dummie_nc a character string
#' @return No return value, called to save a csv file
#' @keywords internal

cdo_csv <- function(dummie_nc){
  dummie_table <- fread(dummie_nc)
  setnames(dummie_table, c("date", "value"))
  fwrite(dummie_table, dummie_nc)
}