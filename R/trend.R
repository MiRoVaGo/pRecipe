#' Trends
#'
#' The function \code{trend} computes linear slope.
#' 
#' @details
#' If `x` is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#' 
#' If `x` is a filename, it should point to a *.nc file.
#' 
#' @import data.table doParallel foreach parallel
#' @importFrom methods setGeneric setMethod
#' @importFrom raster brick calc nlayers
#' @importFrom stats lm
#' @param x Raster* object; data.table (see details); filename (character, see details)
#' @return Raster* object; data.table
#' @export

setGeneric("trend", function(x) standardGeneric("trend"))

#' @rdname trend
#' @method trend Raster

setMethod("trend", "Raster",
          function(x) {
            dummie_time <- 1:nlayers(x)
            X <- cbind(1, dummie_time)
            invXtX <- solve(t(X) %*% X) %*% t(X)
            quickfun <- function(y) (invXtX %*% y)[2]
            dummie <- calc(x, quickfun)
            return(dummie)
          })

#' @rdname trend
#' @method trend data.table

setMethod("trend", "data.table",
          function(x) {
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
            registerDoParallel(cores = no_cores)
            x <- split(x, by = c("lon", "lat"))
            dummie <- foreach (idx = 1:length(x), .combine = rbind) %dopar% {
              dummie_row <- x[[idx]]
              dummie_time <- 1:nrow(dummie_row)
              X <- cbind(1, dummie_time)
              invXtX <- solve(t(X) %*% X) %*% t(X)
              dummie_slope <- (invXtX %*% dummie_row$value)[2]
              dummie_row <- unique(dummie_row[, .(lon, lat)])
              dummie_row$slope <- dummie_slope
              return(dummie_row)
            }
            return(dummie)
          })

#' @rdname trend
#' @method trend character

setMethod("trend", "character",
          function(x) {
            x <- brick(x)
            dummie_time <- 1:nlayers(x)
            X <- cbind(1, dummie_time)
            invXtX <- solve(t(X) %*% X) %*% t(X)
            quickfun <- function(y) (invXtX %*% y)[2]
            dummie <- calc(x, quickfun)
            return(dummie)
          })
