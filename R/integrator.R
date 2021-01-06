#' Data base time splitter
#'
#' Function for splitting the database into common time periods.
#'
#' @param folder_path a character string with the path to the "database" folder.

split_time <- function(folder_path){
  dummie_list <- list.files(folder_path, full.names = TRUE)
  lapply(dummie_list, function(names){
    data_name <- sub(".*/", "", names)
    precip <- readRDS(names)
    if (is.element(data_name, c("20cr.Rds"))) saveRDS(precip[year(Z) >= 1836 & year(Z) <= 1890], paste0(folder_path, "/../integration/1836_1890/", data_name))
    if (is.element(data_name, c("20cr.Rds", "gpcc.Rds"))) saveRDS(precip[year(Z) >= 1891 & year(Z) <= 1899], paste0(folder_path, "/../integration/1891_1899/", data_name))
    if (is.element(data_name, c("20cr.Rds", "ghcn.Rds", "gpcc.Rds", "udel.Rds"))) saveRDS(precip[year(Z) == 1900], paste0(folder_path, "/../integration/1900/", data_name))
    if (is.element(data_name, c("20cr.Rds", "cru_ts.Rds", "ghcn.Rds", "gpcc.Rds", "udel.Rds"))) saveRDS(precip[year(Z) >= 1901 & year(Z) <= 1947], paste0(folder_path, "/../integration/1901_1947/", data_name))
    if (is.element(data_name, c("20cr.Rds", "cru_ts.Rds", "ghcn.Rds", "gpcc.Rds", "ncep_ncar.Rds", "precl.Rds", "udel.Rds"))) saveRDS(precip[year(Z) >= 1948 & year(Z) <= 1978], paste0(folder_path, "/../integration/1948_1978/", data_name))
    if (is.element(data_name, c("20cr.Rds", "cmap.Rds", "cpc.Rds", "cru_ts.Rds", "ghcn.Rds", "gpcc.Rds", "gpcp.Rds", "ncep_doe.Rds", "precl.Rds", "udel.Rds"))) saveRDS(precip[year(Z) >= 1979 & year(Z) <= 1997], paste0(folder_path, "/../integration/1979_1997/", data_name))
    if (is.element(data_name, c("20cr.Rds", "cmap.Rds", "cpc.Rds", "cru_ts.Rds", "ghcn.Rds", "gpcc.Rds", "gpcp.Rds", "ncep_doe.Rds", "precl.Rds", "trmm_3b43.Rds", "udel.Rds"))) saveRDS(precip[year(Z) >= 1998 & year(Z) <= 2000], paste0(folder_path, "/../integration/1998_2000/", data_name))
    if (is.element(data_name, c("20cr.Rds", "cmap.Rds", "cpc.Rds", "cru_ts.Rds", "ghcn.Rds", "gpcc.Rds", "gpcp.Rds", "gpm_imerg.Rds", "ncep_doe.Rds", "precl.Rds", "trmm_3b43.Rds", "udel.Rds"))) saveRDS(precip[year(Z) >= 2001 & year(Z) <= 2011], paste0(folder_path, "/../integration/2001_2011/", data_name))
    if (is.element(data_name, c("20cr.Rds", "cmap.Rds", "cpc.Rds", "cru_ts.Rds", "ghcn.Rds", "gpcc.Rds", "gpcp.Rds", "gpm_imerg.Rds", "ncep_doe.Rds", "trmm_3b43.Rds", "udel.Rds"))) saveRDS(precip[year(Z) >= 2012 & year(Z) <= 2015], paste0(folder_path, "/../integration/2012_2015/", data_name))
    if (is.element(data_name, c("cmap.Rds", "cpc.Rds", "cru_ts.Rds", "gpcc.Rds", "gpcp.Rds", "gpm_imerg.Rds", "ncep_doe.Rds", "trmm_3b43.Rds", "udel.Rds"))) saveRDS(precip[year(Z) == 2016], paste0(folder_path, "/../integration/2016/", data_name))
    if (is.element(data_name, c("cmap.Rds", "cpc.Rds", "cru_ts.Rds", "gpcp.Rds", "gpm_imerg.Rds", "ncep_doe.Rds", "trmm_3b43.Rds", "udel.Rds"))) saveRDS(precip[year(Z) == 2017], paste0(folder_path, "/../integration/2017/", data_name))
    if (is.element(data_name, c("cmap.Rds", "cpc.Rds", "cru_ts.Rds", "gpcp.Rds", "gpm_imerg.Rds", "ncep_doe.Rds", "trmm_3b43.Rds"))) saveRDS(precip[year(Z) >= 2018 & year(Z) <= 2019], paste0(folder_path, "/../integration/2018_2019/", data_name))
  })
  return(invisible())
}

#' Data integrator by period.
#'
#' Function for merging the data sets overlapping by time periods.
#'
#' @param folder_path a character string with the path to the "integration" folder.

merge_time <- function(folder_path){
  dummie_names <- list.files(folder_path, full.names = TRUE) %>% grep(pattern = ".Rds", invert = TRUE, value = TRUE) %>% lapply(list.files, full.names = TRUE)
  dummie_table <- dummie_names[[1]] %>% readRDS()
  dummie_table[, Z := as.yearmon(Z)]
  saveRDS(dummie_table[, -5], paste0(folder_path, "/precip01.Rds"))
  rm(dummie_table)
  gc()
  for (index in 2:length(dummie_names)){
    dummie_list <- dummie_names[[index]] %>% lapply(readRDS) %>% rbindlist()
    dummie_list <- split(dummie_list, year(dummie_list$Z))
    if (index == 6){
      dummie_list_1 <- dummie_list[1:10]
      dummie_list_2 <- dummie_list[11:19]
      rm(dummie_list)
      gc()
      no_cores <- detectCores() - 1
      if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
      cluster <- makeCluster(no_cores, type = "PSOCK")
      clusterExport(cluster, varlist = "dt_parallel")
      clusterEvalQ(cluster, library("data.table"))
      precip_1 <- parLapply(cluster, dummie_list_1, dt_parallel)
      stopCluster(cluster)
      rm(dummie_list_1)
      gc()
      precip_1 <- rbindlist(precip_1)
      cluster <- makeCluster(no_cores, type = "PSOCK")
      clusterExport(cluster, varlist = "dt_parallel")
      clusterEvalQ(cluster, library("data.table"))
      precip_2 <- parLapply(cluster, dummie_list_2, dt_parallel)
      stopCluster(cluster)
      rm(dummie_list_2)
      gc()
      precip_2 <- rbindlist(precip_2)
      precip <- rbind(precip_1, precip_2)
      rm(precip_1, precip_2)
      gc()
      saveRDS(precip, paste0(folder_path, "/precip", str_pad(index, 2, pad = "0"), ".Rds"))
      rm(precip)
      gc()
    }
    no_cores <- detectCores() - 1
    if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
    cluster <- makeCluster(no_cores, type = "PSOCK")
    clusterExport(cluster, varlist = "dt_parallel")
    clusterEvalQ(cluster, library("data.table"))
    precip <- parLapply(cluster, dummie_list, dt_parallel)
    stopCluster(cluster)
    rm(dummie_list)
    gc()
    precip <- rbindlist(precip)
    saveRDS(precip, paste0(folder_path, "/precip", str_pad(index, 2, pad = "0"), ".Rds"))
    rm(precip)
    gc()
  }
  return(invisible())
}