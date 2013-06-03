context("transition_probability.R")

test_that("testing",{
  off  <- locus( 1:2 )
  mom  <- locus( c(1,1) )
  dad1 <- locus( c(2,2) )
  dad2 <- locus( c(1,2) )
  dad3 <- locus( c(1,1) )

  expect_that( transition_probability(), throws_error() ) 
  expect_that( transition_probability( off, mom, locus() ), shows_message() )
  
  t1 <- transition_probability( off, mom, dad1 )
  t2 <- transition_probability( off, mom, dad2 )
  t3 <- transition_probability( off, mom, dad3 )
  
  expect_that( t1, equals(1.0) )
  expect_that( t2, equals(0.5) )
  expect_that( t3, equals(0.0) )

})