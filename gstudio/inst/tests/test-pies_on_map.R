context("pies_on_map.R")
rm(list=ls())

test_that("testing",{
  loc <- locus( 1:2 )
  pop <- data.frame(Population=1:4,loc=c(loc,loc,loc,loc))
  
  expect_that( pies_on_map(FALSE), throws_error() )
  
  expect_that( pies_on_map(pop), throws_error() )
  expect_that( pies_on_map(pop,stratum="Population"), throws_error() )
  
})
