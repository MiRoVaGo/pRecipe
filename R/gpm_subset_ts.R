#' Monthly precipitation data
#'
#' A subset of GPM-IMERGM monthly precipitation data in mm over 2-28E, 42-58N. More details of the raw data can be found \href{https://gpm.nasa.gov/data/imerg}{here}.
#'
#' @format A data.table with 120 obs. of 4 variables:
#' \describe{
#'   \item{date}{Date format \%Y-\%m-\%d}
#'   \item{value}{monthly average values}
#'   \item{name}{full name of the data set}
#'   \item{type}{source type of the data set}
#' }
#' @source National Aeronautics and Space Administration (NASA)
"gpm_subset_ts"