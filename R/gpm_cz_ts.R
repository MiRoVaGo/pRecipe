#' Monthly precipitation data
#'
#' A subset of GPM-IMERGM monthly precipitation data in mm over Czechia. More details of the raw data can be found \href{https://gpm.nasa.gov/data/imerg}{here}.
#'
#' @format A data.table with 120 obs. of 4 variables:
#' \describe{
#'   \item{date}{IDate format \%Y-\%m-\%d}
#'   \item{value}{monthly average values}
#'   \item{name}{full name of the data set}
#'   \item{type}{source type of the data set}
#' }
#' @source National Aeronautics and Space Administration (NASA)
"gpm_cz_ts"