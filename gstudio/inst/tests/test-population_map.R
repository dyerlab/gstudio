context("population_map.R")


test_that("checking",{
  coords <- data.frame(Stratum=c("A","B"), Longitude=c(-77.4548,-77.4547), Latitude=c(37.5460,37.5459) )
  map <- population_map( coords , zoom = 13)
  
  expect_that( inherits(map,"ggmap"), is_true() )
  expect_that( inherits(map,"raster"), is_true() )

})