## ----start, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  eval = TRUE,
  fig.width = 7,
  warning = FALSE,
  message = FALSE
)
library(pRecipe)
library(kableExtra)
data('gpm_global_ts')
data('gpm_subset_ts')
data('gpm_cz_ts')

## ----gauge, echo=FALSE, results = 'asis'--------------------------------------
tibble::tribble(
  ~"Data Set", ~"Spatial Resolution", ~Global, ~Land, ~Ocean, ~"Temporal Resolution", ~"Record Length", ~"Get Data", ~"Reference",
"CPC-Global", "0.5°", "", "x", "", "Daily", "1979/01-2022/08", "[Download](https://psl.noaa.gov/data/gridded/data.cpc.globalprecip.html)", "@xie_cpc_2010",
"CRU TS v4.06", "0.5°", "", "x", "", "Monthly", "1901/01-2021/12", "[Download](https://crudata.uea.ac.uk/cru/data/hrg/)", "@harris_version_2020",
"EM-EARTH", "0.1°", "", "x", "", "Daily", "1950/01-2019/12", "[Download](https://www.frdr-dfdr.ca/repo/dataset/8d30ab02-f2bd-4d05-ae43-11f4a387e5ad)", "@tang_em-earth_2022",
"GHCN v2", "5°", "", "x", "", "Monthly", "1900/01-2015/05", "[Download](https://psl.noaa.gov/data/gridded/data.ghcngridded.html)", "@peterson_overview_1997",
"GPCC v2020", "0.25°", "", "x", "", "Monthly", "1891/01-2022/08", "[Download](https://psl.noaa.gov/data/gridded/data.gpcc.html)", "@schneider_gpcc_2011",
"PREC/L", "0.5°", "", "x", "", "Monthly", "1948/01-2022/08", "[Download](https://psl.noaa.gov/data/gridded/data.precl.html)", "@chen_global_2002",
"UDel v5.01", "0.5°", "", "x", "", "Monthly", "1901/01-2017/12", "[Download](https://psl.noaa.gov/data/gridded/data.UDel_AirT_Precip.html)", "@willmott_terrestrial_2001"
) |>
  kbl(align = 'lcccccccr') |>
  kable_styling("striped") |>
  add_header_above(c(" " = 1, " " = 1, "Spatial Coverage" = 3, " " = 1, " " = 1, " " = 1, " " = 1)) |>
  unclass() |> cat()

## ----satellite, echo=FALSE, results = 'asis'----------------------------------
tibble::tribble(
  ~"Data Set", ~"Spatial Resolution", ~Global, ~Land, ~Ocean, ~"Temporal Resolution", ~"Record Length", ~"Get Data", ~Reference, 
"CHIRPS v2.0", "0.05°", "", "50°SN", "", "Monthly", "1981/01-2022/07", "[Download](https://www.chc.ucsb.edu/data/chirps)", "@funk_climate_2015",
"CMAP", "2.5°", "x", "x", "x", "Monthly", "1979/01-2022/07", "[Download](https://psl.noaa.gov/data/gridded/data.cmap.html)", "@xie_global_1997",
"CMORPH", "0.25°", "60°SN", "60°SN", "60°SN", "Daily", "1998/01-2021/12", "[Download](https://www.ncei.noaa.gov/data/cmorph-high-resolution-global-precipitation-estimates/)", "@joyce_cmorph_2004",
"GPCP v2.3", "0.5°", "x", "x", "x", "Monthly", "1979/01-2022/05", "[Download](https://psl.noaa.gov/data/gridded/data.gpcp.html)", "@adler_global_2018",
"GPM IMERGM v06", "0.1°", "x", "x", "x", "Monthly", "2000/06-2020/12", "[Download](https://doi.org/10.5067/GPM/IMERG/3B-MONTH/06)", "@huffman_gpm_2019",
"MSWEP v2.8", "0.1°", "x", "x", "x", "Monthly", "1979/02-2022/06", "[Download](https://www.gloh2o.org/mswep/)", "@beck_mswep_2019",
"PERSIANN-CDR", "0.25°", "60°SN", "60°SN", "60°SN", "Monthly", "1983/01-2022/06", "[Download](https://chrsdata.eng.uci.edu/)", "@ashouri_persiann-cdr_2015",
"TRMM 3B43 v7", "0.25°", "50°SN", "50°SN", "50°SN", "Monthly", "1998/01-2019/12", "[Download](https://doi.org/10.5067/TRMM/TMPA/MONTH/7)", "@huffman_trmm_2010"
) |>
  kbl(align = 'lcccccccr') |>
  kable_styling("striped") |>
  add_header_above(c(" " = 1, " " = 1, "Spatial Coverage" = 3, " " = 1, " " = 1, " " = 1, " " = 1)) |>
  unclass() |> cat()

## ----reanalysis, echo=FALSE, results = 'asis'---------------------------------
tibble::tribble(
  ~"Data Set", ~"Spatial Resolution", ~Global, ~Land, ~Ocean, ~"Temporal Resolution", ~"Record Length", ~"Get Data", ~Reference,
"20CR v3", "1°", "x", "x", "x", "Monthly", "1836/01-2015/12", "[Download](https://psl.noaa.gov/data/gridded/data.20thC_ReanV3.html)", "@slivinski_towards_2019",
"ERA-20C", "1.125°", "x", "x", "x", "Monthly", "1900/01-2010/12", "[Download](https://www.ecmwf.int/en/forecasts/datasets/reanalysis-datasets/era-20c)", "@poli_era-20c_2016",
"ERA5", "0.25°", "x", "x", "x", "Monthly", "1959/01-2021/12", "[Download](https://www.ecmwf.int/en/forecasts/datasets/reanalysis-datasets/era5)", "@hersbach_era5_2020",
"JRA-55", "1.25°", "x", "x", "x", "Monthly", "1958/01-2021/12", "[Download](https://rda.ucar.edu/datasets/ds628.1/dataaccess/)", "@kobayashi_jra-55_2015",
"MERRA-2", "0.5° x 0.625°", "x", "x", "x", "Monthly", "1980/01-2023/01", "[Download](https://disc.gsfc.nasa.gov/datasets?page=1&project=MERRA-2)", "@gelaro_modern-era_2017",
"NCEP/NCAR R1", "1.875°", "x", "x", "x", "Monthly", "1948/01-2022/08", "[Download](https://psl.noaa.gov/data/gridded/data.ncep.reanalysis.derived.html)", "@kalnay_ncepncar_1996",
"NCEP/DOE R2", "1.875°", "x", "x", "x", "Monthly", "1979/01-2022/08", "[Download](https://psl.noaa.gov/data/gridded/data.ncep.reanalysis2.html)", "@kanamitsu_ncepdoe_2002"
) |>
  kbl(align = 'lcccccccr') |>
  kable_styling("striped") |>
  add_header_above(c(" " = 1, " " = 1, "Spatial Coverage" = 3, " " = 1, " " = 1, " " = 1, " " = 1)) |>
  unclass() |> cat()

## ----models, echo=FALSE, results = 'asis'-------------------------------------
tibble::tribble(
  ~"Data Set", ~"Spatial Resolution", ~Global, ~Land, ~Ocean, ~"Temporal Resolution", ~"Record Length", ~"Get Data", ~Reference,
"FLDAS", "0.1°", "", "x", "", "Monthly", "1982/01-2021/12", "[Download](https://ldas.gsfc.nasa.gov/fldas/fldas-data-download)", "@mcnally_land_2017",
"GLDAS CLSM v2.0", "0.25°", "", "x", "", "Daily", "1948/01-2014/12", "[Download](https://ldas.gsfc.nasa.gov/gldas/gldas-get-data)", "@rodell_global_2004",
"GLDAS NOAH v2.0", "0.25°", "", "x", "", "Monthly", "1948/01-2014/12", "[Download](https://ldas.gsfc.nasa.gov/gldas/gldas-get-data)", "@rodell_global_2004",
"GLDAS VIC v2.0", "1°", "", "x", "", "Monthly", "1948/01-2014/12", "[Download](https://ldas.gsfc.nasa.gov/gldas/gldas-get-data)", "@rodell_global_2004",
"TerraClimate", "4$km$", "", "x", "", "Monthly", "1958/01-2021/12", "[Download](https://www.climatologylab.org/terraclimate.html)", "@abatzoglou_terraclimate_2018"
) |>
  kbl(align = 'lcccccccr') |>
  kable_styling("striped") |>
  add_header_above(c(" " = 1, " " = 1, "Spatial Coverage" = 3, " " = 1, " " = 1, " " = 1, " " = 1)) |>
  unclass() |> cat()

## ----install, eval = FALSE----------------------------------------------------
#  install.packages('pRecipe')
#  library(pRecipe)

## ----download, eval = FALSE---------------------------------------------------
#  download_data(dataset = 'gpm-imerg')
#  gpm_global <- raster::brick('gpm-imerg_tp_mm_global_200006_202012_025_monthly.nc')
#  infoNC(gpm_global)

## ----subset, eval = FALSE-----------------------------------------------------
#  gpm_subset <- subset_data(gpm_global, box = c(-96, -30, -56, 24), yrs = c(2001, 2015))
#  infoNC(gpm_subset)

## ----crop, eval = FALSE-------------------------------------------------------
#  gpm_bol <- crop_data(gpm_subset, "gadm41_BOL_0.shp")
#  infoNC(gpm_bol)

## ----global_ts, eval = FALSE--------------------------------------------------
#  gpm_global_ts <- fldmean(gpm_global)
#  head(gpm_global_ts, 12)

## ----subset_ts, eval = FALSE--------------------------------------------------
#  gpm_subset_ts <- fldmean(gpm_subset)
#  head(gpm_subset_ts, 12)

## ----bol_ts, eval = FALSE-----------------------------------------------------
#  gpm_bol_ts <- fldmean(gpm_bol)
#  head(gpm_bol_ts, 12)

## ----map_global, eval = FALSE-------------------------------------------------
#  plot_map(gpm_global[[1]])

## ----map_subset, eval = FALSE-------------------------------------------------
#  plot_map(gpm_subset[[1]])

## ----map_cz, eval = FALSE-----------------------------------------------------
#  plot_map(gpm_bol[[1]])

## ----lines--------------------------------------------------------------------
plot_line(gpm_global_ts)
plot_line(gpm_subset_ts)
plot_line(gpm_bol_ts)

## ----hearmaps-----------------------------------------------------------------
plot_heatmap(gpm_global_ts)
plot_heatmap(gpm_subset_ts)
plot_heatmap(gpm_bol_ts)

## ----boxplots-----------------------------------------------------------------
plot_box(gpm_global_ts)
plot_box(gpm_subset_ts)
plot_box(gpm_bol_ts)

## ----histograms---------------------------------------------------------------
plot_density(gpm_global_ts)
plot_density(gpm_subset_ts)
plot_density(gpm_bol_ts)

## ----summary, eval=FALSE------------------------------------------------------
#  plot_summary(gpm_global_ts)
#  #plot_summary(gpm_subset_ts)
#  #plot_summary(gpm_cz_ts)

