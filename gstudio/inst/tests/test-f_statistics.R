context("f_statistics.R")

test_that("Inbreeding",{
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
  
  f <- Fis( loci )
  expect_that( f, is_equivalent_to(0.2))  
  
  data <- data.frame( A=loci, B=loci, C=loci)
  f <- Fis( data )
  expect_that( dim(f), is_equivalent_to( c(3,2) ) )
  expect_that( f, is_a("data.frame"))
  expect_that( f$Fis, is_equivalent_to( c(0.2,0.2,0.2)))

})