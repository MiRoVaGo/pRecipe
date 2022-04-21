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