import_20cr <- function(file_path){
  if (!is.character(file_path)) stop ("file_path should be a character string.")
  file_name <- list.files(file_path, full.names = TRUE)
  file_url_base <- "ftp://ftp2.psl.noaa.gov/Datasets/20thC_ReanV3/Monthlies/sfcSI-MO/"
  file_url <- paste0(file_url_base, file_name)
  file_file_path <- paste0(file_path, "/", file_name)
  download.file(file_url, file_file_path)
}

files <- list.files()
ncopen(file_name)
ncin <- paste0("../data/20CRv2/prate.", year_select, ".nc") %>% nc_open()
precip <- ncvar_get(ncin, 'prate') *86400 %>% brick()