#' Trends
#'
#' Function to compute linear slope of each raster grid cell
#' 
#' @import data.table
#' @importFrom methods as is
#' @importFrom raster brick calc nlayers setZ subset zApply
#' @importFrom utils tail
#' @param x a character string with the path to the data file. Or a RasterBrick.
#' @param annual a character string with the desired aggregation function. Suitable options are:
#' \itemize{
#' \item "max"
#' \item "mean"
#' \item "median"
#' \item "min"
#' \item "sum"
#' }
#' @return a RasterLayer with trend values
#' @export

brick_trends <- function(x, annual = NULL){
  if (is.character(x)){
    dummie_brick <- brick(x)
  } else {
    dummie_brick <- x
  }
  if(!is.null(annual)){
    dummie_dates <- as.Date(names(dummie_brick), format = "X%Y.%m.%d")
    dummie_start <- dummie_dates[1]
    dummie_end <- tail(dummie_dates, 1)
    if ((month(dummie_start) != 1) & (month(dummie_end) != 12)){
      start_year <- as.Date(paste0(year(dummie_start) + 1,'-01-01'), format = '%Y-%m-%d')
      end_year <- as.Date(paste0(year(dummie_end) - 1,'-12-01'), format = '%Y-%m-%d')
    } else if ((month(dummie_start) != 1) & (month(dummie_end) == 12)){
      start_year <- as.Date(paste0(year(dummie_start) + 1,'-01-01'), format = '%Y-%m-%d')
      end_year <- dummie_end
    } else if ((month(dummie_start) == 1) & (month(dummie_end) != 12)){
      start_year <- dummie_start
      end_year <- as.Date(paste0(year(dummie_end) - 1,'-12-01'), format = '%Y-%m-%d')
    } else {
      start_year <- dummie_start
      end_year <- dummie_end
    }
    dummie_brick <- subset(dummie_brick, which(getZ(dummie_brick) >= start_year & (getZ(dummie_brick) <= end_year)))
    dummie_brick <- setZ(dummie_brick, seq(start_year, end_year, by = 'month'))
    dummie_brick <- zApply(dummie_brick, by = year,
                           fun = match.fun(annual), na.rm = TRUE)
    dummie_brick <- setZ(dummie_brick, seq(start_year, end_year, by = 'year'))
  }
  dummie_time <- 1:nlayers(dummie_brick)
  X <- cbind(1, dummie_time)
  invXtX <- solve(t(X) %*% X) %*% t(X)
  quickfun <- function(y) (invXtX %*% y)[2]
  dummie_slopes <- calc(dummie_brick, quickfun)
  return(dummie_slopes)
}