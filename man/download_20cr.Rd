% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_20cr.R
\name{download_20cr}
\alias{download_20cr}
\title{20CR data downloader}
\usage{
download_20cr(folder_path = ".", domain = "raw", time_res = "monthly")
}
\arguments{
\item{folder_path}{a character string with the path where the data will be downloaded.}

\item{domain}{a character string with the desired domain data set. Suitable options are:
\itemize{
\item{"raw" for default available spatial coverage,}
\item{"global" for data sets with global (land and ocean) coverage,}
\item{"land" for data sets with land only coverage,}
\item{"ocean", for data sets with ocean only coverage.}
}}

\item{time_res}{a character string with the desired time resolution. Suitable options are:
\itemize{
\item{"daily",}
\item{"monthly",}
\item{"yearly".}
}}
}
\value{
No return value, called to download the data set.
}
\description{
Function for downloading 20CR v3.
}
\keyword{internal}
