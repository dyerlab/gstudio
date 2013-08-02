context("strata_coordinates.R")


test_that("checking",{
  pop <- data.frame( Population=rep( c("A","B"), each=2), Latitude=c(0,0,1,1), Longitude=c(1,1,0,0) )
  coords <- strata_coordinates( pop )
  
  
  expect_that( coords, is_a("data.frame") )
  expect_that( coords$Longitude, is_equivalent_to( c(1,0) ) )
  expect_that( coords$Latitude, is_equivalent_to( c(0,1) ) )
})