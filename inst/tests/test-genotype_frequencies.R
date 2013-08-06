context("genotype_frequencies.R")

test_that("testing",{
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
  
  expect_that( genotype_frequencies(), throws_error() )
  expect_that( genotype_frequencies("bob"), throws_error() )
  
  ret <- genotype_frequencies( loci )
  expect_that( ret, is_a("data.frame"))
  expect_that( dim(ret), is_equivalent_to(c(6,3)))
  expect_that( names(ret), is_equivalent_to(c("Genotype","Observed","Expected")))
  expect_that( all(ret$Genotype == c("1:1","1:2","2:2","2:3","1:3","3:3")), is_true() )
  expect_that( all(ret$Observed == c(2,4,4,1,0,0)), is_true() )
  expect_that( (sum(ret[,2]) - sum(ret[,3])) < 1e-10, is_true())
})