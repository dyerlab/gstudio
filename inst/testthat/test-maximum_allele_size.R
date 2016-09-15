context("maximum_allele_size.R")

test_that("checking",{
  loci <- c( locus( c(1,12) ),
             locus( c(2,22) ),
             locus( c(2222,2) ))

  expect_error( maximum_allele_size() )
  expect_error( maximum_allele_size("A") )
  expect_error( maximum_allele_size(23) )
  expect_error( maximum_allele_size( list() ) )

  sz <- maximum_allele_size(loci)
  expect_that( sz, equals(4) )

}
)