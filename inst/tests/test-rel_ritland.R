context("rel_nason.R")

test_that("Inbreeding",{
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  AC <- locus( c("A","C") )
  BC <- locus( c("B","C") )
  CC <- locus( c("C","C") )
  x <- c(AA,AA,AB,BB,CC,AB,AC,BB,BC,CC)
  
  expect_that( f <- rel_nason( x,allele="X" ), throws_error() )
  
  f <- rel_nason( x,allele="A" )
  expect_that( is(f,"matrix"), is_true())
  expect_that( dim(f), is_equivalent_to( c(10,10)))
  expect_that( f[1,10], is_equivalent_to( 0.1/3) )

  f1 <- rel_nason( x )
  x[2] <- NA
  expect_message(f2<-rel_nason(x))
})