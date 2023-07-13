#' Transform raster into data.table
#'
#' Function to transform a raster brick into a data.table
#'
#' @import data.table doParallel foreach parallel
#' @importFrom methods as setGeneric setMethod
#' @importFrom raster as.data.frame brick nlayers
#' @param x Raster* object; filename (character, see details)
#' @return data.table
#' @export
#' @examples
#' \dontrun{
#' download_data("gldas-vic", tempdir(), timestep = "yearly")
#' r <- raster::brick(paste0(tempdir(),
#' "/gldas-vic_tp_mm_land_194801_201412_025_yearly.nc"))
#' s <- tabular(r)
#' }

setGeneric("tabular", function(x) standardGeneric("tabular"))

#' @rdname tabular
#' @method tabular Raster

setMethod("tabular", "Raster",
          function(x) {
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
            registerDoParallel(cores = no_cores)
            dummie <- foreach (idx = 1:nlayers(x), .combine = rbind) %dopar% {
              dummie_layer <- x[[idx]]
              dummie_layer <- as.data.frame(dummie_layer, xy = TRUE,
                                            long = TRUE, na.rm = TRUE) %>%
                as.data.table()
              setnames(dummie_layer, c("lon", "lat", "date", "value"))
              dummie_layer
            }
            return(dummie)
          })

#' @rdname tabular
#' @method tabular character

setMethod("tabular", "character",
          function(x) {
            dummie_brick <- brick(x)
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
            registerDoParallel(cores = no_cores)
            dummie <- foreach (idx = 1:nlayers(dummie_brick), .combine = rbind) %dopar% {
              dummie_layer <- dummie_brick[[idx]]
              dummie_layer <- as.data.frame(dummie_layer, xy = TRUE,
                                            long = TRUE, na.rm = TRUE) %>%
                as.data.table()
              setnames(dummie_layer, c("lon", "lat", "date", "value"))
              dummie_layer
            }
            return(dummie)
          })
