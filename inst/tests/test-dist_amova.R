context("dist_amova.R")


test_that("strata",{
  
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  AC <- locus( c("A","C") )
  AD <- locus( c("A","D") )
  BC <- locus( c("B","C") )
  BD <- locus( c("B","D") )
  CC <- locus( c("C","C") )
  CD <- locus( c("C","D") )
  DD <- locus( c("D","D") )
  
  loci <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD) 
  D <- dist_amova( loci )
  
  expect_that( D[1,1], equals(0) )
  expect_that( D[1,2], equals(1) )
  expect_that( D[1,3], equals(1) )
  expect_that( D[1,4], equals(1) )
  expect_that( D[1,5], equals(4) )
  expect_that( D[1,6], equals(3) )
  expect_that( D[1,7], equals(3) )
  expect_that( D[1,8], equals(4) )
  expect_that( D[1,9], equals(3) )
  expect_that( D[1,10], equals(4) )
  expect_that( D[2,9], equals(2) ) 

})

