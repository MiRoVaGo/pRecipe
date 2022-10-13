#' Parallel aggregate
#'
#' Function to aggregate a raster brick
#'
#' @import  parallel
#' @importFrom methods as
#' @importFrom raster aggregate as.list brick setZ
#' @param dummie_nc a character string
#' @param new_res numeric
#' @return raster brick
#' @keywords internal

aggregate_brick <- function(dummie_nc, new_res){
  dummie_brick <- brick(dummie_nc)
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
  if (!Reduce("|", grepl("^X\\d\\d\\d\\d\\.\\d\\d\\.\\d\\d", 
                         dummie_names))) {
    if (grepl("persiann", dummie_nc)) {
      dummie_names <- sub("^.", "", dummie_names)
      dummie_names <- as.numeric(dummie_names)
      dummie_Z <- as.Date(dummie_names, origin = "1983-01-01 00:00:00")
    } else if (grepl("gldas-clsm", dummie_nc)) {
      dummie_names <- sub("^.", "", dummie_names)
      dummie_names <- as.numeric(dummie_names)
      dummie_Z <- as.Date(dummie_names, origin = "1948-01-01 00:00:00")
    }
  } else {
    dummie_Z <- as.Date(dummie_names, format = "X%Y.%m.%d")
  }
  dummie_list <- setZ(dummie_list, dummie_Z)
  return(dummie_list)
}