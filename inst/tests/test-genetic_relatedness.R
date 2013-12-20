context("genetic_relatedness.R")


test_that("error checks",{

  
  expect_that( genetic_relatedness( data.frame(X=1)), throws_error() )
  expect_that( genetic_relatedness( numeric(10), mode="Ritland96"), throws_error() )
  
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
  
  expect_that( genetic_relatedness( loci, mode="Bob"), throws_error() )
  expect_that( genetic_relatedness( loci, mode="LynchRitland"), gives_warning() )
  
  r <- genetic_relatedness( loci )
  expect_that( r, is_a("matrix") )
  expect_that( dim(r), is_equivalent_to(c(10,10)))
  expect_that( sum(diag(r)), is_equivalent_to(0))
  expect_that( sum( r - t(r)), is_equivalent_to(0))
  
})


