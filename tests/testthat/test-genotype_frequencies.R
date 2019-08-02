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
  expect_that( genotype_frequencies(loci), gives_warning())
  ret <- genotype_frequencies( loci,supress_warnings = TRUE )
  expect_that( ret, is_a("data.frame"))
  expect_that( dim(ret), is_equivalent_to(c(6,3)))
  expect_that( names(ret), is_equivalent_to(c("Genotype","Observed","Expected")))
  expect_true( all(ret$Genotype == c("1:1","1:2","2:2","2:3","1:3","3:3")) )
  expect_true( all(ret$Observed == c(2,4,4,1,0,0)))
  expect_true( (sum(ret[,2]) - sum(ret[,3])) < 1e-10)
})