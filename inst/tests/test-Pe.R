context("Pe.R")

test_that("checking",{
  loci <- c( locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(1,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ),
             locus( c(2,3) ))
  
  p <- frequencies( loci )$Frequency
  
  expect_that( Pe("Bob"), throws_error() )
  expect_that( Pe(loci), is_equivalent_to( sum( p*(1-p) ) ) )

}
)