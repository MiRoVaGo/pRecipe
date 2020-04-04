capture <- function(name, destination){
  '%!in%' <- function(x, y)!('%in%'(x, y)) # keep function inside for the 'parallel' package
  if (name %!in% c("ghcn", "gpcc", "gpcc_daily", "precl", "udel", "cpc_global", "persiann", 
                   "persiann_ccs", "gpcp", "cmap", "ncep1", "ncep2", "20crv3")){
    stop("Error: Data set not supported. Select one of ghcn, gpcc, gpcc_daily, precl, udel, 
         cpc_global, persiann, persiann_ccs, gpcp, cmap, ncep1, ncep2, 20crv3")
  }
  switch(name,
         "ghcn" = download_ghcn(destination),
         "gpcc" = download_gpcc(destination),
         "gpcc_daily" = download_gpcc_daily(destination),
         "precl" = download_precl(destination),
         "udel" = download_udel(destination),
         "cpc_global" = download_cpc_global(destination),
         "persiann" = download_persiann(destination),
         "persiann_ccs" = download_persiann_ccs(destination),
         "gpcp" = download_gpcp(destination),
         "cmap" = download_cmap(destination),
         "ncep1" = download_ncep1(destination),
         "ncep2" = download_ncep2(destination),
         "20crv3" = download_20crv3(destination)
  )
}