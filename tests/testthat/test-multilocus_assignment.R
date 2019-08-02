context("multilocus_assignment.R")

test_that("checking",{
  
  freqs <- data.frame( Stratum=rep(c("A","B","C"),each=2), Locus="Locus1")
  freqs$Allele <- rep( c("A","B"), times=3)
  freqs$Frequency <- c( 0.1, 0.9, 0.4,0.6, 0.7, 0.3)
  loci <- c( locus(c("A","A")), locus(c("A","B")), locus(c("B","B")) )
  individuals <- data.frame( ID=1:3, Locus1=loci)
  
  
  A <- multilocus_assignment(individuals[1,],freqs)
  expect_true( is(A,"data.frame"), "Not a data.frame" )
  expect_that( dim(A), is_equivalent_to(c(3,3)), "data.frame wrong size")
  expect_that( A[1,2], is_equivalent_to(0.49), "bad frequency")
  

  A <-   multilocus_assignment(individuals[2,],freqs, F=1)
  expect_true( is(A,"data.frame"), "Not a data.frame" )
  expect_that( dim(A), is_equivalent_to(c(0,3)), "data.frame wrong size")
  

})