context("plot_surface_and_points.R")

test_that("shortest paths", {
  require(raster)
  require(ggplot2)
  surface <- raster( matrix(c(1,3,4,3,1,2,3,4,1,2,2,3,3,2,3,1), nrow=4) )
  coords <- data.frame( X=c( 0.15, 0.95 ),Y=c( 0.15, 0.95 ) ) 
  
  
  p <- surface_point_plot( surface, coords )
  
  expect_that( p, is_a("ggplot") )
  expect_that( p, is_a("gg") )
  
})