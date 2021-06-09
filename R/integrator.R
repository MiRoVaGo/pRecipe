#' Data integrator for 1836_1890
#'
#' Function for merging the data sets overlapping by time periods.
#'
#' @param folder_path a character string with the path to the "database" folder.
#' @return No return value, called for side effects.

merge_1836_1890 <- function(folder_path){
  sd_20cr(paste0(folder_path, "/../integration/aux"))
  dummie_years <- seq(1836, 1890)
  dummie_list_values <- grep(paste(dummie_years, collapse = "|"), list.files(folder_path, full.names = TRUE), value = TRUE)
  dummie_list_sd <- grep(paste(dummie_years, collapse = "|"), list.files(paste0(folder_path, "/../integration/aux"), full.names = TRUE), value = TRUE)
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  clusterExport(cluster, c("dummie_list_values", "dummie_list_sd", "folder_path"), envir = environment())
  clusterEvalQ(cluster, library("data.table"))
  parLapply(cluster, dummie_years, function(year){
    dummie_value <- grep(year, dummie_list_values, value = TRUE)
    dummie_value <- readRDS(dummie_value)
    dummie_value <- dummie_value[,-5]
    setnames(dummie_value, "value", "wvalue")
    dummie_sd <- grep(year, dummie_list_sd, value = TRUE)
    dummie_sd <- readRDS(dummie_sd)
    dummie_sd <- dummie_sd[,-5]
    setnames(dummie_sd, "value", "sum_err")
    dummie_table <- merge(dummie_value, dummie_sd, by = c("x", "y", "Z"))
    saveRDS(dummie_table, paste0(folder_path, "/../integration/pRecipe_", year, ".Rds"))
  })
  stopCluster(cluster)
  return(invisible())
}

#' Data integrator for 1891_2020.
#'
#' Function for merging the data sets overlapping by time periods.
#'
#' @param folder_path a character string with the path to the "database" folder.
#' @return No return value, called for side effects.

merge_1891_2020 <- function(folder_path){
  dummie_years <- seq(1891, 2020)
  no_cores <- detectCores() - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster <- makeCluster(no_cores, type = "PSOCK")
  clusterExport(cluster, "folder_path", envir = environment())
  clusterExport(cluster, varlist = "dt_parallel")
  clusterEvalQ(cluster, library("data.table"))
  parLapply(cluster, dummie_years, function(year){
    if (year >= 1979){
      dummie_table <- grep(year, list.files(folder_path, full.names = TRUE), value = TRUE)
      dummie_table <- grep("ncep_ncar", dummie_table, value = TRUE, invert = TRUE)
    } else {
      dummie_table <- grep(year, list.files(folder_path, full.names = TRUE), value = TRUE)
    }
    dummie_table <- lapply(dummie_table, readRDS)
    dummie_table <- rbindlist(dummie_table)
    dummie_table <- dt_parallel(dummie_table)
    saveRDS(dummie_table, paste0(folder_path, "/../integration/pRecipe_", year, ".Rds"))
  })
  stopCluster(cluster)
  return(invisible())
}