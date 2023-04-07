#' Complement make_ts generated csv file
#'
#' Function to generate name and type columns
#'
#' @import data.table
#' @param dummie_name a character string
#' @return character string
#' @keywords internal

aux_ts <- function(dummie_name){
  if (dummie_name == "20cr"){
    dummie <- c("20CR v3", "Reanalysis")
    } else if (dummie_name == "chirps"){
      dummie <- c("CHIRPS v2.0", "Satellite-based")
    } else if (dummie_name == "cmap"){
      dummie <- c("CMAP", "Satellite-based")
    } else if (dummie_name == "cmorph"){
      dummie <- c("CMORPH", "Satellite-based")
    } else if (dummie_name == "cpc"){
      dummie <- c("CPC-Global", "Gauge-based")
    } else if (dummie_name == "cru-ts"){
      dummie <- c("CRU TS v4.06", "Gauge-based")
    } else if (dummie_name == "em-earth"){
      dummie <- c("EM-EARTH", "Gauge-based")
    } else if (dummie_name == "era20c"){
      dummie <- c("ERA-20C", "Reanalysis")
    } else if (dummie_name == "era5"){
      dummie <- c("ERA5", "Reanalysis")
    } else if (dummie_name == "ghcn"){
      dummie <- c("GHCN v2", "Gauge-based")
    } else if (dummie_name == "gldas-clsm"){
      dummie <- c("GLDAS CLSM v2.0", "Model forcing")
    } else if (dummie_name == "gldas-noah"){
      dummie <- c("GLDAS NOAH v2.0", "Model forcing")
    } else if (dummie_name == "gldas-vic"){
      dummie <- c("GLDAS VIC v2.0", "Model forcing")
    } else if (dummie_name == "gpcc"){
      dummie <- c("GPCC v2020", "Gauge-based")
    } else if (dummie_name == "gpcp"){
      dummie <- c("GPCP v2.3", "Satellite-based")
    } else if (dummie_name == "gpm-imerg"){
      dummie <- c("GPM IMERGM v06", "Satellite-based")
    } else if (dummie_name == "mswep"){
      dummie <- c("MSWEP v2.8", "Satellite-based")
    } else if (dummie_name == "ncep-doe"){
      dummie <- c("NCEP/DOE R2", "Reanalysis")
    } else if (dummie_name == "ncep-ncar"){
      dummie <- c("NCEP/NCAR R1", "Reanalysis")
    } else if (dummie_name == "persiann"){
      dummie <- c("PERSIANN-CDR", "Satellite-based")
    } else if (dummie_name == "precl"){
      dummie <- c("PREC/L", "Gauge-based")
    } else if (dummie_name == "terraclimate"){
      dummie <- c("TerraClimate", "Model forcing")
    } else if (dummie_name == "trmm-3b43"){
      dummie <- c("TRMM 3B43 v7", "Satellite-based")
    } else if (dummie_name == "udel"){
      dummie <- c("UDel v5.01", "Gauge-based")
    } else if (dummie_name == "jra55"){
      dummie <- c("JRA-55", "Reanalysis")
    } else if (dummie_name == "merra2"){
      dummie <- c("MERRA-2", "Reanalysis")
    } else if (dummie_name == "fldas"){
      dummie <- c("FLDAS", "Model forcing")
    } else {
      dummie <- c(NA, NA)
    }
  return(dummie)
}