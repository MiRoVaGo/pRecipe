#pRecipe 3.0.0
* renamed and restructured various functions
* added `label`, `muldpm`, `tabular`, and `trend` functions
* `crop_data` now removes empty space with NAs
* `subset_data` now handles subsetting in time and/or space
* improved parallel processing
* functions now have methods for Raster* object, data.table, and filename (character)
* updated vignette
* updated README
* updated documentation accordingly

#pRecipe 2.5.0
* added `%>%` operator
* increased versatility of all plot functions (for evapoRe compatibility)
* fixed bugs for make_ts() not generating a Date column
* updated documentation accordingly

#pRecipe 2.4.0

* yearly data has been added to the database
* added pod, far, csi, and nse functions
* updated the vignette
* updated citation info
* updated documentation accordingly

# pRecipe 2.3.0

* saving is now optional
* save_nc is exported now
* added fldas, jra55, and merra2 data
* land/ocean cropped data has been added to the database
* plot_line now facets by data set type
* updated documentation accordingly

# pRecipe 1.0.0

* pRecipe no longer depends on CDO
* Added function to generate Taylor diagrams
* Updated documentation accordingly

# pRecipe 0.4.2

* Fixed a naming bug that generated ".nc.nc" file extensions
* Updated download links to the latest database

# pRecipe 0.4.0

* pRecipe now is windows friendly
* Updated documentation accordingly

# pRecipe 0.3.0

* Major reworks pRecipe has fewer dependencies
* Simplified database
* No need to reformat the data
* Updated documentation accordingly

# pRecipe 0.2.0

* `import_subset_data` optimized for parallel computing
* several download functions were updated to capture new repository urls
* pRecipe no longer depends on gdalUtils
* added vignette

# pRecipe 0.1.1

* `import_subset_data` now performs in parallel

# pRecipe 0.1.0

* Fixed HTTPS authentication issue when downloading *gpm_imergm* or *trmm_3b43*.
* `download_data` now performs multiple simultaneous downloads when there is more than one file per product, i.e., when downloading *cpc*, *gpm_imergm*, or *trmm_3b43*.