#' Parallel aggregate
#'
#' Function to aggregate a raster brick
#'
#' @import  parallel
#' @importFrom methods as
#' @importFrom raster aggregate as.list brick setZ
#' @param x a character string
#' @param new_res numeric
#' @return raster brick
#' @keywords internal

aggregate_brick <- function(x, new_res){
  if (is.character(x)){
    dummie_brick <- brick(x)
  } else {
    dummie_brick <- x
  }
  dummie_brick <- as.list(dummie_brick)
  no_cores <- detectCores() - 1
  if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  clusterExport(cluster, "new_res", envir = environment())
  dummie_list <- parLapply(cluster, dummie_brick, function(dummie_layer){
    dummie_res <- raster::res(dummie_layer)[1]
    dummie_factor <- new_res/dummie_res
    dummie_raster <- raster::aggregate(dummie_layer, fact = dummie_factor,
                                       fun = mean, na.rm = TRUE)
    dummie_raster
  })
  stopCluster(cluster)
  dummie_list <- brick(dummie_list)
  dummie_names <- names(dummie_list)
  dummie_Z <- as.Date(dummie_names, format = "X%Y.%m.%d")
  dummie_list <- setZ(dummie_list, dummie_Z)
  return(dummie_list)
}