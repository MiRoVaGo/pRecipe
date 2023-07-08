#' Monthly precipitation data
#'
#' Global GPM-IMERGM monthly precipitation data in mm. More details of the raw data can be found \href{https://gpm.nasa.gov/data/imerg}{here}.
#'
#' @format A data.table with 256 obs. of 2 variables:
#' \describe{
#'   \item{date}{IDate format \%Y-\%m-\%d}
#'   \item{value}{monthly average values}
#' }
#' @source National Aeronautics and Space Administration (NASA)
"gpm_global_ts"