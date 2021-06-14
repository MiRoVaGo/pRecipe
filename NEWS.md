# pRecipe 0.1.0

* Fixed HTTPS authentication issue when downloading *gpm_imergm* or *trmm_3b43*.
* `download_data` now performs multiple simultaneous downloads when there is more than one file per product, i.e., when downloading *cpc*, *gpm_imergm*, or *trmm_3b43*.