test_that("pRecipe works", {
  dummie_table <- raster::raster(xmn=-0, xmx=360, ymn=-90, ymx=90, ncols=720, nrows=360)
  dummie_table <- raster::setValues(dummie_table, 1:(raster::ncell(dummie_table)))
  dummie_table <- raster::as.data.frame(dummie_table, xy = TRUE, long = TRUE, na.rm = TRUE)
  dummie_table <- data.table::as.data.table(dummie_table)
  dummie_table$name <- "test"
  data.table::setnames(dummie_table, "layer", "Z")
  expect_output(str(dummie_table), "259200 obs")
})