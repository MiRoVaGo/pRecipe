#' Yearly <stat>
#'
#' The function \code{yearstat} aggregates the data from monthly to yearly.
#'
#' @details
#' If `x` is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#' 
#' If `x` is a filename, it should point to a *.nc file.
#' 
#' `stat` is a character string describing the desired aggregation function. Suitable options are:
#' \itemize{
#' \item "max"
#' \item "mean"
#' \item "median"
#' \item "min"
#' \item "sum" (default)
#' }
#' 
#' @import data.table
#' @importFrom methods as setGeneric setMethod
#' @importFrom raster brick getZ setZ zApply
#' @param x Raster* object; data.table (see details); filename (character, see details)
#' @param stat character
#' @return Raster* object; data.table
#' @export
#' @examples
#' \dontrun{
#' download_data("gldas-vic", path = tempdir())
#' r <- raster::brick(paste0(tempdir(),
#' "/gldas-vic_tp_mm_land_194801_201412_025_monthly.nc"))
#' s <- yearstat(r, "mean")
#' }

setGeneric("yearstat", function(x, stat = "sum") standardGeneric("yearstat"))

#' @rdname yearstat
#' @method yearstat Raster

setMethod("yearstat", "Raster",
          function(x, stat = "sum") {
            dummie_dates <- getZ(x) %>% aux_date()
            start_date <- dummie_dates[1]
            final_date <- tail(dummie_dates, 1)
            if ((month(start_date) != 1) & (month(final_date) != 12)){
              start_date <- as.Date(paste0(year(start_date) + 1, "-01-01"),
                                    format = "%Y-%m-%d")
              final_date <- as.Date(paste0(year(final_date) - 1, "-12-01"),
                                    format = '%Y-%m-%d')
            } else if ((month(start_date) != 1) & (month(final_date) == 12)){
              start_date <- as.Date(paste0(year(start_date) + 1, "-01-01"),
                                    format = '%Y-%m-%d')
            } else if ((month(start_date) == 1) & (month(final_date) != 12)){
              final_date <- as.Date(paste0(year(final_date) - 1, "-12-01"),
                                    format = '%Y-%m-%d')
            }
            dummie <- selyear(x, c(year(start_date), year(final_date)))
            dummie <- zApply(dummie, by = year, fun = match.fun(stat),
                             na.rm = TRUE)
            dummie <- setZ(dummie, seq(start_date, final_date, by = 'year'))
            return(dummie)
          })

#' @rdname yearstat
#' @method yearstat data.table

setMethod("yearstat", "data.table",
          function(x, stat = "sum") {
            dummie_dates <- x$date
            start_date <- min(dummie_dates, na.rm = TRUE)
            final_date <- max(dummie_dates, na.rm = TRUE)
            if ((month(start_date) != 1) & (month(final_date) != 12)){
              start_date <- as.Date(paste0(year(start_date) + 1, "-01-01"),
                                    format = "%Y-%m-%d")
              final_date <- as.Date(paste0(year(final_date) - 1, "-12-01"),
                                    format = '%Y-%m-%d')
            } else if ((month(start_date) != 1) & (month(final_date) == 12)){
              start_date <- as.Date(paste0(year(start_date) + 1, "-01-01"),
                                    format = '%Y-%m-%d')
            } else if ((month(start_date) == 1) & (month(final_date) != 12)){
              final_date <- as.Date(paste0(year(final_date) - 1, "-12-01"),
                                    format = '%Y-%m-%d')
            }
            dummie <- x[(date >= start_date) & (date <= final_date),
                        .(value = match.fun(stat)(value, na.rm = TRUE)),
                        .(lon, lat, year(date))]
            return(dummie)
          })

#' @rdname yearstat
#' @method yearstat character

setMethod("yearstat", "character",
          function(x, stat = "sum") {
            x <- brick(x)
            dummie_dates <- getZ(x) %>% aux_date()
            start_date <- dummie_dates[1]
            final_date <- tail(dummie_dates, 1)
            if ((month(start_date) != 1) & (month(final_date) != 12)){
              start_date <- as.Date(paste0(year(start_date) + 1, "-01-01"),
                                    format = "%Y-%m-%d")
              final_date <- as.Date(paste0(year(final_date) - 1, "-12-01"),
                                    format = '%Y-%m-%d')
            } else if ((month(start_date) != 1) & (month(final_date) == 12)){
              start_date <- as.Date(paste0(year(start_date) + 1, "-01-01"),
                                    format = '%Y-%m-%d')
            } else if ((month(start_date) == 1) & (month(final_date) != 12)){
              final_date <- as.Date(paste0(year(final_date) - 1, "-12-01"),
                                    format = '%Y-%m-%d')
            }
            dummie <- selyear(x, c(year(start_date), year(final_date)))
            dummie <- zApply(dummie, by = year, fun = match.fun(stat),
                             na.rm = TRUE)
            dummie <- setZ(dummie, seq(start_date, final_date, by = 'year'))
            return(dummie)
          })
