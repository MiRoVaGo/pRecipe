#' Subset a precipitation data product in time and space
#'
#' The function \code{subset_spacetime} subsets (time and space) the requested data set and stores it in the same location of the input file.
#'
#' @importFrom methods as is
#' @importFrom raster brick crop extent getZ setZ subset
#' @importFrom R.utils getAbsolutePath
#' @param data a character string with the path to the data file. Or a RasterBrick
#' @param years numeric vector. Time range in the form: (start_year, end_year)
#' @param bbox numeric vector. Bounding box in the form: (xmin, xmax, ymin, ymax).
#' @param autosave logical FALSE (default). If TRUE data will be automatically stored in the same location of the input file
#' @return A subsetted RasterBrick.
#' @export
#' @examples
#' \dontrun{
#' subset_spacetime("gpcp_tp_mm_global_197901_202205_025_monthly.nc",
#' c(2000, 2010), c(12.24, 18.85, 48.56, 51.12), autosave = TRUE)
#' subset_spacetime("dummie.nc", c(2000, 2010), 
#' c(12.24, 18.85, 48.56, 51.12), autosave = TRUE)
#' }

subset_spacetime <- function(data, years, bbox, autosave = FALSE){
  nc_in <- getAbsolutePath(data)
  checker <- name_check(data)
  if (checker$length == 8) {
    checker$name[4] <- "subset"
    checker$name[5] <- years[1]
    checker$name[6] <- years[2]
    nc_out <- paste(checker$name, collapse = "_")
    nc_out <- paste0(nc_out, ".nc")
    nc_mid <- sub("(.*/)(.*)", "\\1", nc_in)
    nc_out <- paste0(nc_mid, nc_out)
  } else {
    warning("This is not pRecipe data")
    nc_out <- sub(".nc.*", "", nc_in)
    nc_out <- paste0(nc_out, "_subset.nc")
  }
  nc_out <- sub(".nc.nc.*", ".nc", nc_out)
  check_out <- exists_check(nc_out)
  if (check_out$exists) stop(check_out$sms)
  if (is.character(data)){
    dummie_brick <- brick(nc_in)
  } else {
    dummie_brick <- data
  }
  start_year <- paste0(years[1], "-01-01")
  end_year <- paste0(years[2], "-12-31")
  lonlatbox <- as(extent(bbox[1], bbox[2], bbox[3], bbox[4]),
                  'SpatialPolygons')
  dummie_subset <- crop(dummie_brick, lonlatbox)
  dummie_dates <- getZ(dummie_subset)
  if (is.character(dummie_dates) | is.numeric(dummie_dates)) {
    if (is.numeric(dummie_dates)) {
      dummie_dates <- as.character(dummie_dates)
    } 
    if (length(dummie_dates[dummie_dates == "00"]) >= 1) {
      dummie_dates <- sub("^00$", "", dummie_dates)
      dummie_dates <- dummie_dates[dummie_dates != ""]
      dummie_dates <- as.Date(dummie_dates)
    } else if (!Reduce("|",grepl("-01", dummie_dates))) {
      dummie_dates <- as.numeric(dummie_dates)
      if (grepl("persiann", nc_out)) {
        dummie_origin <- "1983-01-01 00:00:00"
      } else if (grepl("gldas-", nc_out)) {
        dummie_origin <- "1948-01-01 00:00:00"
      } else {
        dummie_origin <- "1970-01-01 00:00:00"
      }
      dummie_dates <- as.Date(dummie_dates, origin = dummie_origin)
    } else {
      dummie_dates <- as.Date(dummie_dates)
    }
  }
  range_years <- which(dummie_dates >= start_year & 
                         (dummie_dates <= end_year))
  dummie_subset <- subset(dummie_subset, range_years)
  if (is(dummie_subset, "RasterStack")) {
    dummie_subset <- brick(dummie_subset)
    dummie_names <- names(dummie_subset)
    if (Reduce("|", grepl("^X\\d\\d\\d\\d\\.\\d\\d\\.\\d\\d", 
                           dummie_names))) {
      dummie_Z <- as.Date(dummie_names, format = "X%Y.%m.%d")
    }
    dummie_subset <- setZ(dummie_subset, dummie_Z)
  }
  if (autosave){
    save_nc(dummie_subset, nc_out)
    fix_name_out(nc_out)
    return(invisible())
  } else {
    return(dummie_subset)
  }
}
