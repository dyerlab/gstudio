context("rel_queller.R")

test_that("relatedness",{
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  AC <- locus( c("A","C") )
  BC <- locus( c("B","C") )
  CC <- locus( c("C","C") )
  x <- c(AA,AA,AB,BB,CC,AB,AC,BB,BC,CC)
  
  expect_that( f <- rel_queller( x,allele="X" ), throws_error() )
  
  f <- rel_queller( x )
  expect_true( is(f,"matrix"))
  expect_that( dim(f), is_equivalent_to( c(10,10)) )
  expect_true( is.na(f[1,1]) ) 
  expect_that( f[1,2], is_equivalent_to( 1 ) )

  f1 <- rel_nason( x )
  x[2] <- NA
  expect_message(f2<-rel_nason(x))
})