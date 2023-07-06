#' Spatial aggregation
#'
#' The function \code{regrid} aggregates data into a new grid resolution.
#'
#' @details
#' If x is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#' 
#' If x is a filename, it should point to a *.nc file.
#' 
#' @import data.table
#' @import sp
#' @importFrom methods setGeneric setMethod
#' @importFrom raster aggregate brick getZ nlayers raster rasterFromXYZ res setZ
#' @param x Raster* object; data.table (see details); filename (character; see details)
#' @param res numeric. Target resolution must be a multiple of 0.25 (e.g., 0.5, 1, 2.5)
#' @return Raster* object; data.table
#' @export
#' @examples
#' \dontrun{
#' x <- regrid("gpcp_tp_mm_global_197901_202205_025_monthly.nc", 1)
#' z <- regrid("dummie.nc", 1)
#' }

setGeneric("regrid", function(x, y) standardGeneric("regrid"))

#' @rdname regrid
#' @method regrid Raster

setMethod("regrid", "Raster",
          function(x, y) {
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
            registerDoParallel(cores = no_cores)
            dummie_dates <- getZ(x) %>% aux_date()
            dummie_res <- res(x)[1]
            dummie_factor <- y/dummie_res
            dummie <- foreach (idx = 1:nlayers(x)) %dopar% {
              dummie_layer <- x[[idx]]
              dummie_layer <- aggregate(dummie_layer, fact = dummie_factor,
                                        fun = mean, na.rm = TRUE)
              dummie_layer
            }
            dummie <- brick(dummie)
            dummie <- setZ(dummie, dummie_dates)
            return(dummie)
          })

#' @rdname regrid
#' @method regrid data.table

setMethod("regrid", "data.table",
          function(x, y) {
            dummie_list <- split(x, by = "date")
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
            registerDoParallel(cores = no_cores)
            dummie_res <- min(diff(sort(unique(x$date))))
            dummie_factor <- y/dummie_res
            dummie <- foreach (idx = 1:length(dummie_list), .combine = rbind) %dopar% {
              dummie_table <- dummie_list[[idx]]
              dummie_date <- unique(dummie_table$date)
              dummie_layer <- dummie_table[, .(lon, lat, value)]
              dummie_layer <- rasterFromXYZ(dummie_layer)
              dummie_layer <- aggregate(dummie_layer, fact = dummie_factor,
                                         fun = mean, na.rm = TRUE)
              dummie_layer <- as.data.frame(dummie_layer, xy = TRUE,
                                            long = TRUE, na.rm = FALSE) %>%
                as.data.table()
              dummie_layer$layer <- dummie_date
              setnames(dummie_layer, c("lon", "lat", "date", "value"))
              dummie_layer
            }
            return(dummie)
          })

#' @rdname regrid
#' @method regrid character

setMethod("regrid", "character",
          function(x, y) {
            dummie_brick <- brick(x)
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
            registerDoParallel(cores = no_cores)
            dummie_dates <- getZ(dummie_brick) %>% aux_date()
            dummie_res <- res(dummie_brick)[1]
            dummie_factor <- y/dummie_res
            dummie <- foreach (idx = 1:nlayers(dummie_brick)) %dopar% {
              dummie_layer <- dummie_brick[[idx]]
              dummie_layer <- aggregate(dummie_layer, fact = dummie_factor,
                                        fun = mean, na.rm = TRUE)
              dummie_layer
            }
            dummie <- brick(dummie)
            dummie <- setZ(dummie, dummie_dates)
            return(dummie)
          })
