context("cov_2_dist.R")

test_that("distance and covariance transformations.",{
  C <- matrix( c(1/3,0,-1/3,0,2/3, -2/3, -1/3, -2/3, 1),ncol=3)
  Dp <- matrix( c(0,1,2,1,0,3,2,3,0),ncol=3)
  D <- cov_2_dist( C ) 
  
  expect_that( D, is_a("matrix"))
  expect_that( sum(diag(D)), equals(0) )
  expect_that( D, is_equivalent_to(Dp) )
  

  expect_that( cov_2_dist( FALSE), throws_error() )
  expect_that( cov_2_dist( matrix(1:10, ncol=2)), throws_error())

  
  
})



