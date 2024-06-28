#' Field mean
#'
#' The function \code{fldmean} computes the spatial weighted average for each timestep.
#'
#' @details
#' If `x` is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#' 
#' If `x` is a filename, it should point to a *.nc file.
#' 
#' @import data.table doParallel foreach parallel sp
#' @importFrom methods setGeneric setMethod
#' @importFrom raster area brick cellStats getZ
#' @param x Raster* object; data.table (see details); filename (character, see details)
#' @return data.table
#' @export
#' @examples
#' \dontrun{
#' download_data("gldas-vic", tempdir(), timestep = "yearly")
#' r <- raster::brick(paste0(tempdir(),
#' "/gldas-vic_tp_mm_land_194801_201412_025_yearly.nc"))
#' s <- fldmean(r)
#' }

setGeneric("fldmean", function(x) standardGeneric("fldmean"))

#' @rdname fldmean
#' @method fldmean Raster

setMethod("fldmean", "Raster",
          function(x){
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
            registerDoParallel(cores = no_cores)
            dummie_dates <- getZ(x) %>% aux_date()
            dummie <- foreach (idx = 1:nlayers(x), .combine = rbind) %dopar% {
              dummie_step <- x[[idx]]
              dummie_area <- area(dummie_step, na.rm = TRUE, weights = TRUE)
              dummie_step <- dummie_area * dummie_step
              dummie_step <- cellStats(dummie_step, stat = "sum", na.rm = TRUE)
              dummie_step <- data.table("date" = dummie_dates[idx],
                                        "value" = dummie_step)
              dummie_step
            }
            return(dummie)
          })

#' @rdname fldmean
#' @method fldmean data.table

setMethod("fldmean", "data.table",
          function(x){
            dummie_list <- unique(x$date)
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
            registerDoParallel(cores = no_cores)
            dummie <- foreach (idx = 1:length(dummie_list), .combine = rbind) %dopar% {
              dummie_table <- x[date == dummie_list[idx]]
              dummie_date <- unique(dummie_table$date)
              dummie_step <- dummie_table[, .(lon, lat, value)]
              dummie_step <- rasterFromXYZ(dummie_step)
              dummie_area <- area(dummie_step, na.rm = TRUE, weights = TRUE)
              dummie_step <- dummie_area * dummie_step
              dummie_step <- cellStats(dummie_step, stat = "sum", na.rm = TRUE)
              dummie_step <- data.table("date" = dummie_date,
                                        "value" = dummie_step)
              dummie_step
            }
            return(dummie)
          })

#' @rdname fldmean
#' @method fldmean character

setMethod("fldmean", "character",
          function(x){
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
            registerDoParallel(cores = no_cores)
            dummie_brick <- brick(x)
            dummie_dates <- getZ(dummie_brick) %>% aux_date()
            dummie <- foreach (idx = 1:nlayers(dummie_brick), .combine = rbind) %dopar% {
              dummie_step <- dummie_brick[[idx]]
              dummie_area <- area(dummie_step, na.rm = TRUE, weights = TRUE)
              dummie_step <- dummie_area * dummie_step
              dummie_step <- cellStats(dummie_step, stat = "sum", na.rm = TRUE)
              dummie_step <- data.table("date" = dummie_dates[idx],
                                        "value" = dummie_step)
              dummie_step
            }
            return(dummie)
          })
