#' Monthly precipitation data
#'
#' A subset of ERA5 monthly precipitation data in mm over Czechia. More details of the raw data can be found \href{https://www.ecmwf.int/en/forecasts/datasets/reanalysis-datasets/era5}{here}.
#'
#' @format A data.table with 480 obs. of 2 variables:
#' \describe{
#'   \item{date}{IDate format \%Y-\%m-\%d}
#'   \item{value}{monthly average values}
#' }
#' @source European Centre for Medium-Range Weather Forecasts (ECMWF)
"era5_cze_ts"