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
    switch(data_name,
    "20cr.Rds" = {saveRDS(precip[year(Z) >= 1836 & year(Z) <= 1890], paste0(folder_path, "/../integration/1836_1890/", data_name))
      saveRDS(precip[year(Z) >= 1891 & year(Z) <= 1899], paste0(folder_path, "/../integration/1891_1899/", data_name))
      saveRDS(precip[year(Z) == 1900], paste0(folder_path, "/../integration/1900/", data_name))
      saveRDS(precip[year(Z) >= 1901 & year(Z) <= 1947], paste0(folder_path, "/../integration/1901_1947/", data_name))
      saveRDS(precip[year(Z) >= 1948 & year(Z) <= 1978], paste0(folder_path, "/../integration/1948_1978/", data_name))
      saveRDS(precip[year(Z) >= 1979 & year(Z) <= 1997], paste0(folder_path, "/../integration/1979_1997/", data_name))
      saveRDS(precip[year(Z) >= 1998 & year(Z) <= 2000], paste0(folder_path, "/../integration/1998_2000/", data_name))
      saveRDS(precip[year(Z) >= 2001 & year(Z) <= 2011], paste0(folder_path, "/../integration/2001_2011/", data_name))
      saveRDS(precip[year(Z) >= 2012 & year(Z) <= 2015], paste0(folder_path, "/../integration/2012_2015/", data_name))},
    "cmap.Rds" = {saveRDS(precip[year(Z) >= 1979 & year(Z) <= 1997], paste0(folder_path, "/../integration/1979_1997/", data_name))
      saveRDS(precip[year(Z) >= 1998 & year(Z) <= 2000], paste0(folder_path, "/../integration/1998_2000/", data_name))
      saveRDS(precip[year(Z) >= 2001 & year(Z) <= 2011], paste0(folder_path, "/../integration/2001_2011/", data_name))
      saveRDS(precip[year(Z) >= 2012 & year(Z) <= 2015], paste0(folder_path, "/../integration/2012_2015/", data_name))
      saveRDS(precip[year(Z) == 2016], paste0(folder_path, "/../integration/2016/", data_name))
      saveRDS(precip[year(Z) == 2017], paste0(folder_path, "/../integration/2017/", data_name))
      saveRDS(precip[year(Z) >= 2018 & year(Z) <= 2019], paste0(folder_path, "/../integration/2018_2019/", data_name))},
    "cpc.Rds" = {saveRDS(precip[year(Z) >= 1979 & year(Z) <= 1997], paste0(folder_path, "/../integration/1979_1997/", data_name))
      saveRDS(precip[year(Z) >= 1998 & year(Z) <= 2000], paste0(folder_path, "/../integration/1998_2000/", data_name))
      saveRDS(precip[year(Z) >= 2001 & year(Z) <= 2011], paste0(folder_path, "/../integration/2001_2011/", data_name))
      saveRDS(precip[year(Z) >= 2012 & year(Z) <= 2015], paste0(folder_path, "/../integration/2012_2015/", data_name))
      saveRDS(precip[year(Z) == 2016], paste0(folder_path, "/../integration/2016/", data_name))
      saveRDS(precip[year(Z) == 2017], paste0(folder_path, "/../integration/2017/", data_name))
      saveRDS(precip[year(Z) >= 2018 & year(Z) <= 2019], paste0(folder_path, "/../integration/2018_2019/", data_name))},
    "cru_ts.Rds" = {saveRDS(precip[year(Z) >= 1901 & year(Z) <= 1947], paste0(folder_path, "/../integration/1901_1947/", data_name))
      saveRDS(precip[year(Z) >= 1948 & year(Z) <= 1978], paste0(folder_path, "/../integration/1948_1978/", data_name))
      saveRDS(precip[year(Z) >= 1979 & year(Z) <= 1997], paste0(folder_path, "/../integration/1979_1997/", data_name))
      saveRDS(precip[year(Z) >= 1998 & year(Z) <= 2000], paste0(folder_path, "/../integration/1998_2000/", data_name))
      saveRDS(precip[year(Z) >= 2001 & year(Z) <= 2011], paste0(folder_path, "/../integration/2001_2011/", data_name))
      saveRDS(precip[year(Z) >= 2012 & year(Z) <= 2015], paste0(folder_path, "/../integration/2012_2015/", data_name))
      saveRDS(precip[year(Z) == 2016], paste0(folder_path, "/../integration/2016/", data_name))
      saveRDS(precip[year(Z) == 2017], paste0(folder_path, "/../integration/2017/", data_name))
      saveRDS(precip[year(Z) >= 2018 & year(Z) <= 2019], paste0(folder_path, "/../integration/2018_2019/", data_name))},
    "ghcn.Rds" = {saveRDS(precip[year(Z) == 1900], paste0(folder_path, "/../integration/1900/", data_name))
      saveRDS(precip[year(Z) >= 1901 & year(Z) <= 1947], paste0(folder_path, "/../integration/1901_1947/", data_name))
      saveRDS(precip[year(Z) >= 1948 & year(Z) <= 1978], paste0(folder_path, "/../integration/1948_1978/", data_name))
      saveRDS(precip[year(Z) >= 1979 & year(Z) <= 1997], paste0(folder_path, "/../integration/1979_1997/", data_name))
      saveRDS(precip[year(Z) >= 1998 & year(Z) <= 2000], paste0(folder_path, "/../integration/1998_2000/", data_name))
      saveRDS(precip[year(Z) >= 2001 & year(Z) <= 2011], paste0(folder_path, "/../integration/2001_2011/", data_name))
      saveRDS(precip[year(Z) >= 2012 & year(Z) <= 2015], paste0(folder_path, "/../integration/2012_2015/", data_name))},
    "gpcc.Rds" = {saveRDS(precip[year(Z) >= 1891 & year(Z) <= 1899], paste0(folder_path, "/../integration/1891_1899/", data_name))
      saveRDS(precip[year(Z) == 1900], paste0(folder_path, "/../integration/1900/", data_name))
      saveRDS(precip[year(Z) >= 1901 & year(Z) <= 1947], paste0(folder_path, "/../integration/1901_1947/", data_name))
      saveRDS(precip[year(Z) >= 1948 & year(Z) <= 1978], paste0(folder_path, "/../integration/1948_1978/", data_name))
      saveRDS(precip[year(Z) >= 1979 & year(Z) <= 1997], paste0(folder_path, "/../integration/1979_1997/", data_name))
      saveRDS(precip[year(Z) >= 1998 & year(Z) <= 2000], paste0(folder_path, "/../integration/1998_2000/", data_name))
      saveRDS(precip[year(Z) >= 2001 & year(Z) <= 2011], paste0(folder_path, "/../integration/2001_2011/", data_name))
      saveRDS(precip[year(Z) >= 2012 & year(Z) <= 2015], paste0(folder_path, "/../integration/2012_2015/", data_name))
      saveRDS(precip[year(Z) == 2016], paste0(folder_path, "/../integration/2016/", data_name))},
    "gpcp.Rds" = {saveRDS(precip[year(Z) >= 1979 & year(Z) <= 1997], paste0(folder_path, "/../integration/1979_1997/", data_name))
      saveRDS(precip[year(Z) >= 1998 & year(Z) <= 2000], paste0(folder_path, "/../integration/1998_2000/", data_name))
      saveRDS(precip[year(Z) >= 2001 & year(Z) <= 2011], paste0(folder_path, "/../integration/2001_2011/", data_name))
      saveRDS(precip[year(Z) >= 2012 & year(Z) <= 2015], paste0(folder_path, "/../integration/2012_2015/", data_name))
      saveRDS(precip[year(Z) == 2016], paste0(folder_path, "/../integration/2016/", data_name))
      saveRDS(precip[year(Z) == 2017], paste0(folder_path, "/../integration/2017/", data_name))
      saveRDS(precip[year(Z) >= 2018 & year(Z) <= 2019], paste0(folder_path, "/../integration/2018_2019/", data_name))},
    "gpm_imergm.Rds" = {saveRDS(precip[year(Z) >= 2001 & year(Z) <= 2011], paste0(folder_path, "/../integration/2001_2011/", data_name))
      saveRDS(precip[year(Z) >= 2012 & year(Z) <= 2015], paste0(folder_path, "/../integration/2012_2015/", data_name))
      saveRDS(precip[year(Z) == 2016], paste0(folder_path, "/../integration/2016/", data_name))
      saveRDS(precip[year(Z) == 2017], paste0(folder_path, "/../integration/2017/", data_name))
      saveRDS(precip[year(Z) >= 2018 & year(Z) <= 2019], paste0(folder_path, "/../integration/2018_2019/", data_name))},
    "ncep_ncar.Rds" = {saveRDS(precip[year(Z) >= 1948 & year(Z) <= 1978], paste0(folder_path, "/../integration/1948_1978/", data_name))},
    "ncep_doe.Rds" = {saveRDS(precip[year(Z) >= 1979 & year(Z) <= 1997], paste0(folder_path, "/../integration/1979_1997/", data_name))
      saveRDS(precip[year(Z) >= 1998 & year(Z) <= 2000], paste0(folder_path, "/../integration/1998_2000/", data_name))
      saveRDS(precip[year(Z) >= 2001 & year(Z) <= 2011], paste0(folder_path, "/../integration/2001_2011/", data_name))
      saveRDS(precip[year(Z) >= 2012 & year(Z) <= 2015], paste0(folder_path, "/../integration/2012_2015/", data_name))
      saveRDS(precip[year(Z) == 2016], paste0(folder_path, "/../integration/2016/", data_name))
      saveRDS(precip[year(Z) == 2017], paste0(folder_path, "/../integration/2017/", data_name))
      saveRDS(precip[year(Z) >= 2018 & year(Z) <= 2019], paste0(folder_path, "/../integration/2018_2019/", data_name))},
    "precl.Rds" = {saveRDS(precip[year(Z) >= 1948 & year(Z) <= 1978], paste0(folder_path, "/../integration/1948_1978/", data_name))
      saveRDS(precip[year(Z) >= 1979 & year(Z) <= 1997], paste0(folder_path, "/../integration/1979_1997/", data_name))
      saveRDS(precip[year(Z) >= 1998 & year(Z) <= 2000], paste0(folder_path, "/../integration/1998_2000/", data_name))
      saveRDS(precip[year(Z) >= 2001 & year(Z) <= 2011], paste0(folder_path, "/../integration/2001_2011/", data_name))},
    "trmm_3b43.Rds" = {saveRDS(precip[year(Z) >= 1998 & year(Z) <= 2000], paste0(folder_path, "/../integration/1998_2000/", data_name))
      saveRDS(precip[year(Z) >= 2001 & year(Z) <= 2011], paste0(folder_path, "/../integration/2001_2011/", data_name))
      saveRDS(precip[year(Z) >= 2012 & year(Z) <= 2015], paste0(folder_path, "/../integration/2012_2015/", data_name))
      saveRDS(precip[year(Z) == 2016], paste0(folder_path, "/../integration/2016/", data_name))
      saveRDS(precip[year(Z) == 2017], paste0(folder_path, "/../integration/2017/", data_name))
      saveRDS(precip[year(Z) >= 2018 & year(Z) <= 2019], paste0(folder_path, "/../integration/2018_2019/", data_name))},
    "udel.Rds" = {saveRDS(precip[year(Z) == 1900], paste0(folder_path, "/../integration/1900/", data_name))
      saveRDS(precip[year(Z) >= 1901 & year(Z) <= 1947], paste0(folder_path, "/../integration/1901_1947/", data_name))
      saveRDS(precip[year(Z) >= 1948 & year(Z) <= 1978], paste0(folder_path, "/../integration/1948_1978/", data_name))
      saveRDS(precip[year(Z) >= 1979 & year(Z) <= 1997], paste0(folder_path, "/../integration/1979_1997/", data_name))
      saveRDS(precip[year(Z) >= 1998 & year(Z) <= 2000], paste0(folder_path, "/../integration/1998_2000/", data_name))
      saveRDS(precip[year(Z) >= 2001 & year(Z) <= 2011], paste0(folder_path, "/../integration/2001_2011/", data_name))
      saveRDS(precip[year(Z) >= 2012 & year(Z) <= 2015], paste0(folder_path, "/../integration/2012_2015/", data_name))
      saveRDS(precip[year(Z) == 2016], paste0(folder_path, "/../integration/2016/", data_name))
      saveRDS(precip[year(Z) == 2017], paste0(folder_path, "/../integration/2017/", data_name))})})
  return(invisible())
}

#' Data integrator for 1836_1890
#'
#' Function for merging the data sets overlapping by time periods.
#'
#' @param folder_path a character string with the path to the "integration" folder.

merge_1836_1890 <- function(folder_path){
  folder_path <- paste0(folder_path, "/1836_1890")
  dummie_names <- list.files(folder_path, full.names = TRUE) %>% readRDS()
  dummie_table[, Z := as.yearmon(Z)]
  saveRDS(dummie_table[, -5], paste0(folder_path, "/precip01.Rds"))
  rm(dummie_table)
  gc()
  return(invisible())
}

#' Data integrator for 1891_1899.
#'
#' Function for merging the data sets overlapping by time periods.
#'
#' @param folder_path a character string with the path to the "integration" folder.

merge_1891_1899 <- function(folder_path){
  folder_path <- paste0(folder_path, "/1891_1899")
  dummie_list <- list.files(folder_path, full.names = TRUE) %>% lapply(readRDS) %>% rbindlist()
  lidummie_list <- split(dummie_list, year(dummie_list$Z))
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
  saveRDS(precip, paste0(folder_path, "/precip02.Rds"))
  rm(precip)
  gc()
  return(invisible())
}

#' Data integrator for 1900.
#'
#' Function for merging the data sets overlapping by time periods.
#'
#' @param folder_path a character string with the path to the "integration" folder.

merge_1900 <- function(folder_path){
  folder_path <- paste0(folder_path, "/1900")
  dummie_list <- list.files(folder_path, full.names = TRUE) %>% lapply(readRDS) %>% rbindlist()
  dummie_list <- split(dummie_list, year(dummie_list$Z))
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
  saveRDS(precip, paste0(folder_path, "/precip03.Rds"))
  rm(precip)
  gc()
  return(invisible())
}

#' Data integrator for 1901_1947.
#'
#' Function for merging the data sets overlapping by time periods.
#'
#' @param folder_path a character string with the path to the "integration" folder.

merge_1901_1947 <- function(folder_path){
  folder_path <- paste0(folder_path, "/1901_1947")
  dummie_list <- list.files(folder_path, full.names = TRUE) %>% lapply(readRDS) %>% rbindlist()
  dummie_list <- split(dummie_list, year(dummie_list$Z))
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
  saveRDS(precip, paste0(folder_path, "/precip04.Rds"))
  rm(precip)
  gc()
  return(invisible())
}

#' Data integrator for 1948_1978.
#'
#' Function for merging the data sets overlapping by time periods.
#'
#' @param folder_path a character string with the path to the "integration" folder.

merge_1948_1978 <- function(folder_path){
  folder_path <- paste0(folder_path, "/1948_1978")
  dummie_list <- list.files(folder_path, full.names = TRUE) %>% lapply(readRDS) %>% rbindlist()
  dummie_list <- split(dummie_list, year(dummie_list$Z))
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
  saveRDS(precip, paste0(folder_path, "/precip05.Rds"))
  rm(precip)
  gc()
  return(invisible())
}

#' Data integrator for 1979_1997.
#'
#' Function for merging the data sets overlapping by time periods.
#'
#' @param folder_path a character string with the path to the "integration" folder.

merge_1979_1997 <- function(folder_path){
  folder_path <- paste0(folder_path, "/1979_1997")
  dummie_list <- list.files(folder_path, full.names = TRUE) %>% lapply(readRDS) %>% rbindlist()
  dummie_list <- split(dummie_list, year(dummie_list$Z))
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
  saveRDS(precip, paste0(folder_path, "/precip06.Rds"))
  rm(precip)
  gc()
  return(invisible())
}

#' Data integrator for 1998_2000
#'
#' Function for merging the data sets overlapping by time periods.
#'
#' @param folder_path a character string with the path to the "integration" folder.

merge_1998_2000 <- function(folder_path){
  folder_path <- paste0(folder_path, "/1998_2000")
  dummie_list <- list.files(folder_path, full.names = TRUE) %>% lapply(readRDS) %>% rbindlist()
  dummie_list <- split(dummie_list, year(dummie_list$Z))
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
  saveRDS(precip, paste0(folder_path, "/precip07.Rds"))
  rm(precip)
  gc()
  return(invisible())
}

#' Data integrator for 2001_2011.
#'
#' Function for merging the data sets overlapping by time periods.
#'
#' @param folder_path a character string with the path to the "integration" folder.

merge_2001_2011 <- function(folder_path){
  folder_path <- paste0(folder_path, "/2001_2011")
  dummie_list <- list.files(folder_path, full.names = TRUE) %>% lapply(readRDS) %>% rbindlist()
  dummie_list <- split(dummie_list, year(dummie_list$Z))
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
  saveRDS(precip, paste0(folder_path, "/precip08.Rds"))
  rm(precip)
  gc()
  return(invisible())
}

#' Data integrator for 2012_2015.
#'
#' Function for merging the data sets overlapping by time periods.
#'
#' @param folder_path a character string with the path to the "integration" folder.

merge_2012_2015 <- function(folder_path){
  folder_path <- paste0(folder_path, "/2012_2015")
  dummie_list <- list.files(folder_path, full.names = TRUE) %>% lapply(readRDS) %>% rbindlist()
  dummie_list <- split(dummie_list, year(dummie_list$Z))
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
  saveRDS(precip, paste0(folder_path, "/precip09.Rds"))
  rm(precip)
  gc()
  return(invisible())
}

#' Data integrator for 2016.
#'
#' Function for merging the data sets overlapping by time periods.
#'
#' @param folder_path a character string with the path to the "integration" folder.

merge_2016 <- function(folder_path){
  folder_path <- paste0(folder_path, "/2016")
  dummie_list <- list.files(folder_path, full.names = TRUE) %>% lapply(readRDS) %>% rbindlist()
  dummie_list <- split(dummie_list, year(dummie_list$Z))
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
  saveRDS(precip, paste0(folder_path, "/precip10.Rds"))
  rm(precip)
  gc()
  return(invisible())
}

#' Data integrator for 2017.
#'
#' Function for merging the data sets overlapping by time periods.
#'
#' @param folder_path a character string with the path to the "integration" folder.

merge_2017 <- function(folder_path){
  folder_path <- paste0(folder_path, "/2017")
  dummie_list <- list.files(folder_path, full.names = TRUE) %>% lapply(readRDS) %>% rbindlist()
  dummie_list <- split(dummie_list, year(dummie_list$Z))
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
  saveRDS(precip, paste0(folder_path, "/precip11.Rds"))
  rm(precip)
  gc()
  return(invisible())
}

#' Data integrator for 2018_2019.
#'
#' Function for merging the data sets overlapping by time periods.
#'
#' @param folder_path a character string with the path to the "integration" folder.

merge_2018_2019 <- function(folder_path){
  folder_path <- paste0(folder_path, "/2018_2019")
  dummie_list <- list.files(folder_path, full.names = TRUE) %>% lapply(readRDS) %>% rbindlist()
  dummie_list <- split(dummie_list, year(dummie_list$Z))
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
  saveRDS(precip, paste0(folder_path, "/precip12.Rds"))
  rm(precip)
  gc()
  return(invisible())
}

#' Data integrator by period.
#'
#' Function for merging the data sets overlapping by time periods.
#'
#' @param folder_path a character string with the path to the "integration" folder.

merge_time <- function(folder_path){
  merge_1836_1890(folder_path)
  merge_1891_1899(folder_path)
  merge_1900(folder_path)
  merge_1901_1947(folder_path)
  merge_1948_1978(folder_path)
  merge_1979_1997(folder_path)
  merge_1998_2000(folder_path)
  merge_2001_2011(folder_path)
  merge_2012_2015(folder_path)
  merge_2016(folder_path)
  merge_2017(folder_path)
  merge_2018_2019(folder_path)
  return(invisible())
}