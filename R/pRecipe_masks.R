#' Masks data
#'
#' Function for various masks.
#'
#' @import data.table
#' @return data.table
#' @export

pRecipe_masks <- function(){
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  zenodo_url <- "https://zenodo.org/record/10196716/files/pRecipe_masks_global_025.gz"
  dummie <- fread(zenodo_url)
  return(dummie)
}
