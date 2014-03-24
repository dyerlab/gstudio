context("strata_coordinates.R")


test_that("checking",{
  pop <- data.frame( Population=rep( c("B","A"), each=2), Longitude=c(1,1,0,0), Latitude=c(0,0,1,1) )
  coords <- strata_coordinates( pop )
  
  
  expect_that( coords, is_a("data.frame") )
  expect_that( names(coords), is_equivalent_to(c("Stratum","Longitude","Latitude")))
  expect_that( as.character(coords$Stratum), is_equivalent_to(c("B","A")))
  expect_that( coords$Longitude, is_equivalent_to( c(1,0) ) )
  expect_that( coords$Latitude, is_equivalent_to( c(0,1) ) )
  
  coords.sp <- strata_coordinates( pop, as.SpatialPoints=TRUE)
  expect_that( coords.sp, is_a("SpatialPoints"))
  
  coords.sort <- strata_coordinates( pop, sort.output=TRUE )
  expect_that( as.character(coords.sort$Stratum), is_equivalent_to(c("A","B")))
  
})