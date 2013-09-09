context("geom_surface.R")

test_that("shortest paths", {
  surface <- raster::raster( matrix(c(1,3,4,3,1,2,3,4,1,2,2,3,3,2,3,1), nrow=4) )
  p <- geom_surface( surface )   
  expect_that( p, is_a("proto") )
  expect_that( p, is_a("environment") )  
})