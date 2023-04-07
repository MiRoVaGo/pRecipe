#' Weighted average
#'
#' Function to compute the weighted average of an .nc file
#'
#' @import data.table parallel
#' @importFrom raster area as.list brick cellStats getZ
#' @param x a character string
#' @return data.table variable
#' @keywords internal

weighted_average <- function(x){
  if (is.character(x)){
    dummie_brick <- brick(x)
  } else {
    dummie_brick <- x
  }
  dummie_dates <- getZ(dummie_brick)
  dummie_brick <- as.list(dummie_brick)
  no_cores <- detectCores() - 1
  if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  dummie_list <- parLapply(cluster, dummie_brick, function(dummie_layer){
    dummie_area <- raster::area(dummie_layer, na.rm = TRUE, weights = TRUE)
    dummie_raster <- dummie_area * dummie_layer
    dummie_raster <- raster::cellStats(dummie_raster, stat = "sum", na.rm = TRUE)
    dummie_raster
  })
  stopCluster(cluster)
  dummie_list <- unlist(dummie_list)
  dummie_table <- data.table("date" = dummie_dates, "value" = dummie_list)
  return(dummie_table)
}