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
  
  loci <- to_mv( c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD) )
  
  expect_that( dist_amova(c(1,1), loci ), equals(0) )
  expect_that( dist_amova(c(1,2), loci ), equals(1) )
  expect_that( dist_amova(c(1,3), loci ), equals(1) )
  expect_that( dist_amova(c(1,4), loci ), equals(1) )
  expect_that( dist_amova(c(1,5), loci ), equals(4) )
  expect_that( dist_amova(c(1,6), loci ), equals(3) )
  expect_that( dist_amova(c(1,7), loci ), equals(3) )
  expect_that( dist_amova(c(1,8), loci ), equals(4) )
  expect_that( dist_amova(c(1,9), loci ), equals(3) )
  expect_that( dist_amova(c(1,10),loci ), equals(4) )
  expect_that( dist_amova(c(2,9), loci ), equals(2) ) 

})

