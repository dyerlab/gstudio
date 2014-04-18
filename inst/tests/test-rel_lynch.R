context("rel_lynch.R")

test_that("Inbreeding",{
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  AC <- locus( c("A","C") )
  BC <- locus( c("B","C") )
  CC <- locus( c("C","C") )
  x <- c(AA,AA,AB,BB,CC,AB,AC,BB,BC,CC)
  
  expect_that( f <- rel_lynch( ), throws_error() )
  expect_that( f <- rel_lynch("B"), throws_error() )
  
  f <- rel_lynch( x )
  expect_that( is(f,"matrix"), is_true())
  expect_that( dim(f), is_equivalent_to( c(10,10)))
  expect_that( f[1,2], is_equivalent_to(1.0) ) 

  x[2] <- NA
  expect_message(f2<-rel_nason(x))
})