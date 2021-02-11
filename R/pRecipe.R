#' Download and import precipitation data from various sources
#'
#' The function \code{download_data} downloads the selected data product.
#'
#' @import data.table gdalUtils ggplot2 ncdf4 parallel rgdal
#' @importFrom dplyr %>% 
#' @importFrom getPass getPass
#' @importFrom lubridate day days_in_month
#' @importFrom methods as is
#' @importFrom raster aggregate as.data.frame as.list brick disaggregate extend extent flip metadata ncell raster rasterFromXYZ res resample setValues setZ t zApply
#' @importFrom rhdf5 h5read
#' @importFrom R.utils gunzip
#' @importFrom sp CRS coordinates over proj4string spTransform
#' @importFrom stats sd
#' @importFrom stringr str_pad
#' @importFrom utils download.file install.packages URLencode
#' @importFrom viridis scale_fill_viridis
#' @importFrom zoo as.yearmon as.Date.yearmon
#' @param project_folder a character string with the path where pRecipe will be hosted. Inside it the required subfolders will be created see \code{\link{create_folders}}
#' @param name a character string with the name(s) of the desired data set. Suitable options are:
#' \itemize{
#' \item{"all" for all of the below listed data sets (default),}
#' \item{"20cr" for 20CR v3,}
#' \item{"cmap" for CMAP standard version,}
#' \item{"cpc" for CPC-Global,}
#' \item{"cru_ts" for CRU_TS v4.04,}
#' \item{"ghcn" for GHCN-M v2}
#' \item{"gpcc" for GPCC v2018,}
#' \item{"gpcp" for GPCP v2.3,}
#' \item{"gpm_imergm" for GPM IMERGM Final v06,}
#' \item{"ncep_ncar" for NCEP/NCAR,}
#' \item{"ncep_doe" for NCEP/DOE,}
#' \item{"precl" for PRECL,}
#' \item{"trmm_3b43" for TRMM 3B43 v7,}
#' \item{"udel" for UDEL v501.}
#' }
#' @param reformat logical. If TRUE the downloaded datasets are reformatted into data.table and stored in .Rds files. See \code{\link{reformat_data}}
#' @export
#' @examples
#' \dontrun{
#' download_data("~/global_precipitation/pRecipe")
#' download_data("~/projects/czu/pRecipe", c("cru_ts", "cpc", "ghcn", "gpcp"), reformat = TRUE)
#' download_data("~/research/pRecipe", c("gpm_imergm", "trmm_3b43"))
#' }

download_data <- function(project_folder, name = "all", reformat = FALSE){
  if (!Reduce("&", is.element(name, c("20cr", "all", "cmap", "cpc", "cru_ts", "ghcn", "gpcc", "gpcp", "gpm_imergm", "ncep_ncar", "ncep_doe", "precl", "trmm_3b43", "udel")))){
    stop("Error: Data set not supported. Select from 20cr, cmap, cpc, cru_ts, ghcn, gpcc, gpcp, gpm_imergm, ncep_ncar, ncep_doe, precl, trmm_3b43, udel")
  }
  create_folders(project_folder)
  destination <- paste0(project_folder,"/data/raw")
  lapply(name, function(dataset) switch(dataset,
         "20cr" = download_20cr(destination),
         "all"  = download_all(destination),
         "cmap" = download_cmap(destination),
         "cpc" = download_cpc(destination),
         "cru_ts" = download_cru_ts(destination),
         "ghcn" = download_ghcn(destination),
         "gpcc" = download_gpcc(destination),
         "gpcp" = download_gpcp(destination),
         "gpm_imergm" = download_gpm_imergm(destination),
         "ncep_ncar" = download_ncep_ncar(destination),
         "ncep_doe" = download_ncep_doe(destination),
         "precl" = download_precl(destination),
         "trmm_3b43" = download_trmm_3b43(destination),
         "udel" = download_udel(destination)
  ))
  if (reformat == TRUE) reformat_data(destination, name)
  return(invisible())
}

#' Reformat the downloaded data sets into .Rds files
#'
#' The function \code{reformat_data} reformats the data sets into monthly total precipitation data.tables at 0.5 degree resolution. 
#'
#' @param name a character string with the name of the desired data set. Suitable options are:
#' \itemize{
#' \item{"all" for all of the below listed data sets (default),}
#' \item{"20cr" for 20CR v3,}
#' \item{"cmap" for CMAP standard version,}
#' \item{"cpc" for CPC-Global,}
#' \item{"cru_ts" for CRU_TS v4.04,}
#' \item{"ghcn" for GHCN-M v2}
#' \item{"gpcc" for GPCC v2018,}
#' \item{"gpcp" for GPCP v2.3,}
#' \item{"gpm_imergm" for GPM IMERGM Final v06,}
#' \item{"ncep" for NCEP/NCAR,}
#' \item{"ncep2" for NCEP/DOE,}
#' \item{"precl" for PRECL,}
#' \item{"trmm_3b43" for TRMM 3B43 v7,}
#' \item{"udel" for UDEL v501.}
#' }
#' @param raw_folder_path a character string with the path where the "raw" folder is located.
#' @export
#' @examples
#' \dontrun{
#' reformat_data("~/global_precipitation/pRecipe/data/raw")
#' reformat_data("~/research/pRecipe/data/raw", c("gpm_imergm", "trmm_3b43"))
#' }

reformat_data <- function(raw_folder_path, name = "all"){
  if (!Reduce("&", is.element(name, c("all", "20cr", "cmap", "cpc", "cru_ts", "ghcn", "gpcc", "gpcp", "gpm_imergm", "ncep_ncar", "ncep_doe", "precl", "trmm_3b43", "udel")))){
    stop("Error: Data set not supported. Select from 20cr, cmap, cpc, cru_ts, ghcn, gpcc, gpcp, gpm_imergm, ncep_ncar, ncep_doe, precl, trmm_3b43, udel")
  }
  if (!grepl("*/data/raw", raw_folder_path)){
    stop("Error: raw_folder_path should point to the location of 'data/raw'")
  }
  lapply(name, function(dataset) switch(dataset,
         "20cr" = reformat_20cr(raw_folder_path),
         "all" = reformat_all(raw_folder_path),
         "cmap" = reformat_cmap(raw_folder_path),
         "cpc" = reformat_cpc(raw_folder_path),
         "cru_ts" = reformat_cru_ts(raw_folder_path),
         "ghcn" = reformat_ghcn(raw_folder_path),
         "gpcc" = reformat_gpcc(raw_folder_path),
         "gpcp" = reformat_gpcp(raw_folder_path),
         "gpm_imergm" = reformat_gpm_imergm(raw_folder_path),
         "ncep_ncar" = reformat_ncep_ncar(raw_folder_path),
         "ncep_doe" = reformat_ncep_doe(raw_folder_path),
         "precl" = reformat_precl(raw_folder_path),
         "trmm_3b43" = reformat_trmm_3b43(raw_folder_path),
         "udel" = reformat_udel(raw_folder_path)
  ))
  return(invisible())
}

#' Read precipitation data.table from database
#'
#' The function \code{import_data} imports the requested data sets.
#'
#' @param name a character string with the name of the desired data set. Suitable options are:
#' \itemize{
#' \item{"all" for all of the below listed data sets,}
#' \item{"20cr" for 20CR v3,}
#' \item{"cmap" for CMAP standard version,}
#' \item{"cpc" for CPC-Global,}
#' \item{"cru_ts" for CRU_TS v4.04,}
#' \item{"ghcn" for GHCN-M v2}
#' \item{"gpcc" for GPCC v2018,}
#' \item{"gpcp" for GPCP v2.3,}
#' \item{"gpm_imergm" for GPM IMERGM Final v06,}
#' \item{"ncep" for NCEP/NCAR,}
#' \item{"ncep2" for NCEP/DOE,}
#' \item{"precl" for PRECL,}
#' \item{"trmm_3b43" for TRMM 3B43 v7,}
#' \item{"udel" for UDEL v501.}
#' }
#' @param database_folder_path a character string with the path where the "database" folder is located.
#' @return a data.table with the requested precipitation data sets.
#' @export
#' @examples
#' \dontrun{
#' x <- import_data("~/global_precipitation/pRecipe/data/database", "all")
#' x <- import_data("~/projects/czu/pRecipe/data/database", c("cru_ts", "cpc", "ghcn", "gpcp"))
#' x <- import_data("~/research/pRecipe/data/database", c("gpm_imergm", "trmm_3b43"))
#' }

import_data <- function(database_folder_path, name){
  if (!Reduce("&", is.element(name, c("20cr", "all", "cmap", "cpc", "cru_ts", "ghcn", "gpcc", "gpcp", "gpm_imergm", "ncep_ncar", "ncep_doe", "precl", "trmm_3b43", "udel")))){
    stop("Error: Data set not supported. Select from 20cr, cmap, cpc, cru_ts, ghcn, gpcc, gpcp, gpm_imergm, ncep_ncar, ncep_doe, precl, trmm_3b43, udel")
  }
  if (!grepl("*/data/database", database_folder_path)){
    stop("Error: database_folder_path should point to the location of 'data/database'")
  }
  if (Reduce("&", name == "all")){
    name <- list.files(database_folder_path, full.names = TRUE)
  } else {
    
    name <- grep(paste(name, collapse = "|"), list.files(database_folder_path, full.names = TRUE), value = TRUE)
  }
  precip <- lapply(name, readRDS) %>% rbindlist()
  return(precip)
}

#' Subset precipitation data sets
#'
#' The function \code{subset_data} subsets the imported data sets.
#'
#' @param x a pRecipe data.table imported using \code{import_data}.
#' @param start_year numeric.
#' @param end_year numeric.
#' @param box numeric vector. Bounding box in the form: (xmin, ymin, xmax, ymax).
#' @return a data.table with the subsetted data sets
#' @export
#' @examples
#' \dontrun{
#' x <- import_data("~/projects/czu/pRecipe/data/database", c("cru_ts", "cpc", "ghcn", "gpcp"))
#' y <- subset_data(x, 2000, 2009, c(12.24, 48.56, 18.85, 51.12))
#' }

subset_data <- function(x, start_year, end_year, box){
  if (!is(x, "pRecipe")) stop("Error: x must be a pRecipe data.table")
  x <- x[year(Z) >= start_year & year(Z) <= end_year & x >= box[1] & x <= box[3] & y >= box[2] & y <= box[4]]
  return(x)
}

#' Resampling precipitation data sets
#'
#' The function \code{resample_data} resamples the imported data.
#'
#' @param x a pRecipe data.table imported using \code{import_data}.
#' @param yearly logical. If TRUE (default) monthly data will be aggregated into yearly.
#' @param resolution numeric. Desired spatial resolution (original is 0.5)
#' @return a data.table with the resampled data sets
#' @export
#' @examples
#' \dontrun{
#' x <- import_data("~/projects/czu/pRecipe/data/database", c("cru_ts", "cpc", "ghcn", "gpcp"))
#' y <- resample_data(x, yearly = FALSE, 5)
#' z <- resample_data(x, yearly = TRUE, 2.5)
#' }

resample_data <- function(x, yearly = TRUE, resolution){
  if (!is(x, "pRecipe")) stop("Error: x must be a pRecipe data.table")
  if (yearly == TRUE){
    x <- x[, value := sum(value, na.rm = TRUE), by = .(x, y, Z)]
    x <- split(x, x$name)
    no_cores <- detectCores() - 1
    if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
    cluster <- makeCluster(no_cores, type = "PSOCK")
    clusterExport(cluster, varlist = c("dt_aggregate", "resolution"))
    precip <- parLapply(cluster, x, function(dataset){
      precip <- dt_aggregate(x, resolution)
    })
  }
  return(x)
}

#' Crop precipitation data sets
#'
#' The function \code{crop_data} crops the data sets using a shapefile mask.
#'
#' @param x a pRecipe data.table imported using \code{\link{import_data}}.
#' @param shp_path a character string with the path to the ".shp" file.
#' @return a data.table with the cropped data sets
#' @export
#' @examples
#' \dontrun{
#' x <- import_data("~/projects/czu/pRecipe/data/database", c("cru_ts", "cpc", "ghcn", "gpcp"))
#' y <- subset_data(x, 2000, 2009, c(12.24, 48.56, 18.85, 51.12))
#' w <- crop_data(x, "~/Downloads/cze.shp")
#' z <- crop_data(y, "~/Downloads/cze.shp")
#' }

crop_data <- function(x, shp_path){
  if (!is(x, "pRecipe")) stop("Error: x must be a pRecipe data.table")
  shp_mask <- readOGR(shp_path)
  shp_mask <- spTransform(shp_mask, "+proj=longlat +datum=WGS84 +ellps=WGS84")
  x <- as.data.frame(x)
  sp::coordinates(x) <- ~ x + y
  sp::proj4string(x) <- proj4string(shp_mask)
  x <- x[!is.na(over(x, as(shp_mask, "SpatialPolygons"))), ]
  x <- as.data.table(x)
  class(x) <- append(class(x),"pRecipe")
  return(x)
}