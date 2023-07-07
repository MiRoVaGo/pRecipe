#' Field mean
#'
#' The function \code{fldmean} computes the spatial weighted average for each timestep.
#'
#' @details
#' If x is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#' 
#' If x is a filename, it should point to a *.nc file.
#' 
#' If y is provided additional info (data set name and type) will be added and the return value will be of class pRecipe. Available options are:
#' \itemize{
#' \item{"20cr" for 20CR v3,}
#' \item{"chirps" for CHIRPS v2.0,}
#' \item{"cmap" for CMAP standard version,}
#' \item{"cmorph" for CMORPH,}
#' \item{"cpc" for CPC-Global,}
#' \item{"cru-ts" for CRU_TS v4.06,}
#' \item{"em-earth" for EM-EARTH,}
#' \item{"era20c" for ERA-20C,}
#' \item{"era5" for ERA5,}
#' \item{"fldas" for FLDAS,}
#' \item{"ghcn" for GHCN-M v2,}
#' \item{"gldas-clsm" for GLDAS CLSM,}
#' \item{"gldas-noah" for GLDAS NOAH,}
#' \item{"gldas-vic" for GLDAS VIC,}
#' \item{"gleam" for GLEAM v3.7a,}
#' \item{"gpcc" for GPCC v2020,}
#' \item{"gpcp" for GPCP v2.3,}
#' \item{"gpm_imerg" for GPM IMERGM Final v06,}
#' \item{"jra55" for JRA-55,}
#' \item{"merra2" for MERRA-2,}
#' \item{"mswep" for MSWEP v2.8,}
#' \item{"ncep-doe" for NCEP/DOE,}
#' \item{"ncep-ncar" for NCEP/NCAR,}
#' \item{"persiann" for PERSIANN-CDR,}
#' \item{"precl" for PREC/L,}
#' \item{"terraclimate" for TerraClimate,}
#' \item{"trmm-3b43" for TRMM 3B43 v7,}
#' \item{"udel" for UDEL v501.}
#' }
#' 
#' @import data.table doParallel foreach parallel sp
#' @importFrom methods setGeneric setMethod
#' @importFrom raster area brick cellStats getZ
#' @param x Raster* object; data.table (see details); filename (character, see details)
#' @param y character (optional, see details)
#' @return data.table; pRecipe
#' @export
#' @examples
#' \dontrun{
#' download_data("gldas-vic", tempdir(), timestep = "yearly")
#' r <- raster::brick(paste0(tempdir(),
#' "/gldas-vic_tp_mm_land_194801_201412_025_yearly.nc"))
#' s <- fldmean(r, "gldas-vic")
#' }

setGeneric("fldmean", function(x, y = "") standardGeneric("fldmean"))

#' @rdname fldmean
#' @method fldmean Raster

setMethod("fldmean", "Raster",
          function(x, y = ""){
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
            registerDoParallel(cores = no_cores)
            dummie_dates <- getZ(x) %>% aux_date()
            dummie <- foreach (idx = 1:nlayers(x), .combine = rbind) %dopar% {
              dummie_step <- x[[idx]]
              dummie_area <- area(dummie_step, na.rm = TRUE, weights = TRUE)
              dummie_step <- dummie_area * dummie_step
              dummie_step <- cellStats(dummie_step, stat = "sum", na.rm = TRUE)
              dummie_step <- data.table("date" = dummie_dates[idx],
                                        "value" = dummie_step)
              dummie_step
            }
            if (y != "") {
              y <- aux_info(y)
              dummie <- pRecipe(dummie, id = y)
            }
            return(dummie)
          })

#' @rdname fldmean
#' @method fldmean data.table

setMethod("fldmean", "data.table",
          function(x, y = ""){
            dummie_list <- split(x, by = "date")
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
            registerDoParallel(cores = no_cores)
            dummie <- foreach (idx = 1:length(dummie_list), .combine = rbind) %dopar% {
              dummie_table <- dummie_list[[idx]]
              dummie_date <- unique(dummie_table$date)
              dummie_step <- dummie_table[, .(lon, lat, value)]
              dummie_step <- rasterFromXYZ(dummie_step)
              dummie_area <- area(dummie_step, na.rm = TRUE, weights = TRUE)
              dummie_step <- dummie_area * dummie_step
              dummie_step <- cellStats(dummie_step, stat = "sum", na.rm = TRUE)
              dummie_step <- data.table("date" = dummie_date,
                                        "value" = dummie_step)
              dummie_step
            }
            if (y != "") {
              y <- aux_info(y)
              dummie <- pRecipe(dummie, id = y)
            }
            return(dummie)
          })

#' @rdname fldmean
#' @method fldmean character

setMethod("fldmean", "character",
          function(x, y = ""){
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
            registerDoParallel(cores = no_cores)
            dummie_brick <- brick(x)
            dummie_dates <- getZ(dummie_brick) %>% aux_date()
            dummie <- foreach (idx = 1:nlayers(dummie_brick), .combine = rbind) %dopar% {
              dummie_step <- dummie_brick[[idx]]
              dummie_area <- area(dummie_step, na.rm = TRUE, weights = TRUE)
              dummie_step <- dummie_area * dummie_step
              dummie_step <- cellStats(dummie_step, stat = "sum", na.rm = TRUE)
              dummie_step <- data.table("date" = dummie_dates[idx],
                                        "value" = dummie_step)
              dummie_step
            }
            if (y != "") {
              y <- aux_info(y)
              dummie <- pRecipe(dummie, id = y)
            }
            return(dummie)
          })
