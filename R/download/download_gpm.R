download_gpm <- function(destination, url_txt){
  if (!is.character(destination)) stop ("destination should be character string.")
  if (!is.character(url_txt)) stop ("url_txt should be character string.")
  file_url <- scan(fil, what = list(""), flush = TRUE) %>% unlist()
  file_name <- substr(file_url, 228, 280)
  for (index in 1:length(file_url)){
    file_destination <- paste0(destination, "/", file_name[index])
    GET(as.character(file_url[index]),
        authenticate("username", "password"),
        write_disk(path = file_destination, overwrite = TRUE))
  }
}

library(readr)
library(data.table)
require(RCurl)
list_url <- fread("ftp_url_001_202009150852.txt", header = FALSE)
destination <- "/Volumes/LaCie/GitHub/pRecipe/R/data/gpm"
for (index in 1:235){
file_url <- URLencode(as.character(list_url[index]))
try(download_gpm(destination, file_url))
}

fil <- "C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_download/subset_GPM_3IMERGDF_06_20200826_193405.txt"
url <- scan(fil, what = list(""), flush = TRUE)
urls <- unlist(url)
#t = basename(urls)
names = substr(urls, 228, 280) #subset the character for naming the files
library(httr)
for (i in 1:length(urls)){
  GET(as.character(urls[i]),
      authenticate("username", "password"),
      write_disk(path = names[i], overwrite = T))
}