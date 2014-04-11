context("genetic_relatedness.R")


test_that("error checks",{

  
  expect_that( genetic_relatedness( data.frame(X=1)), throws_error() )
  expect_that( genetic_relatedness( numeric(10), mode="Ritland"), throws_error() )
  
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
  df <- data.frame( ID=1:10 )
  expect_that( genetic_relatedness(df),throws_error() )
  
  df$TPI <- loci
  r <- genetic_relatedness( df, mode="Nason")
  expect_that( r, is_a("matrix") )
  expect_that( dim(r), is_equivalent_to(c(10,10)))
  expect_that( sum(diag(r)), is_equivalent_to(10))
#   
#   
#   # make the data frame
#   df <- data.frame(TPI=loci)
#   expect_that( genetic_relatedness( df, mode="bob"), throws_error())
#   expect_that( genetic_relatedness( df, mode="LynchRitland"),gives_warning())
#   
#   r2 <- genetic_relatedness( df )
#   expect_that( sum(r-r2), is_equivalent_to(0))
#   expect_that( sum( r - t(r2)), is_equivalent_to(0))
  
})


