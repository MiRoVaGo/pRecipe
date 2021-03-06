% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloaders.R
\name{download_cpc}
\alias{download_cpc}
\title{CPC data downloader}
\usage{
download_cpc(folder_path, start_year = 1979, end_year = 2020)
}
\arguments{
\item{folder_path}{a character string with the path where the "raw" folder is located.}

\item{start_year}{numeric. Start year should be between 1979-2020.}

\item{end_year}{numeric. End year should be between 1979-2020, and should be greater or equal to start year.}
}
\value{
No return value, called to download the data set.
}
\description{
Function for downloading CPC-GLOBAL NC files.
}
