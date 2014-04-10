context("f_ij.R")

test_that("Inbreeding",{
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  AC <- locus( c("A","C") )
  BC <- locus( c("B","C") )
  CC <- locus( c("C","C") )
  loci <- c(AA,AA,AB,BB,CC,AB,AC,BB,BC,CC)
  
  f <- Fij( loci,allele="X" )
  expect_that( is(x,"matrix"), is_true())
  expect_that( dim(x), is_equivalent_to( c(10,10)))
  expect_that( sum(x), is_equivalent_to(0) )
  
  f <- Fij( loci,allele="A" )
  expect_that( is(x,"matrix"), is_true())
  
  f <- Fij( loci )
  expect_that( is(x,"matrix"), is_true())
  

})