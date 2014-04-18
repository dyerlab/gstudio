context("kronecker_delta.R")

test_that("checking",{
  locus1 <- locus( c(1,1) )
  locus2 <- locus( c(1,2) )
  locus3 <- locus( c(2,2) )
  
  expect_that( kronecker_delta(), throws_error() )
  expect_that( kronecker_delta(loc1,c(loc2,loc3)), throws_error() )
  expect_that( kronecker_delta( "A", 23), throws_error() )
 
  k <- kronecker_delta(locus1,locus2)
  expect_that( k, is_equivalent_to( c(1,1,0,1,0,0)))
  k <- kronecker_delta(locus1,locus3)
  expect_that( k, is_equivalent_to( c(1,0,0,0,0,1)))
  k <- kronecker_delta(locus2,locus3)
  expect_that( k, is_equivalent_to( c(0,0,0,1,1,1)))
}
)