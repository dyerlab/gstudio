context("A.R")

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

  expect_that( A("Bob"), throws_error() )
  expect_that( A(loci), is_equivalent_to(3) )
  expect_that( A(loci, min_freq=0.05), equals(2) )
}
)