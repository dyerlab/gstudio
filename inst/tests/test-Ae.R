context("Ae.R")

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
  
  
  expect_that( Ae("Bob"), throws_error() )
  expect_that( Ae(loci)[1,1], is_equivalent_to( 1/(1-He(loci))) )

}
)