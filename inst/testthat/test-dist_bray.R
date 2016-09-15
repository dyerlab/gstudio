context("dist_bray.R")


test_that("individual",{

  expect_that( dist_bray("Bob"), throws_error() )
  expect_that( dist_bray(data.frame(Pop=1)), throws_error() )
  expect_that( dist_bray(data.frame(Pop=1), stratum="bob"), throws_error() )
  
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  AC <- locus( c("A","C") )
  BB <- locus( c("B","B") )
  BC <- locus( c("B","C") )
  CC <- locus( c("C","C") )
  
  
  loci <- c(AA,AA,AB,AA,BB,BC,CC,BB,BB,CC)
  df <- data.frame( Population=c(rep("A",5),rep("B",5) ), TPI=loci )
  D <- dist_bray(df)
  expect_that( D, is_a("matrix") )
  expect_that( dim(D), is_equivalent_to(c(2,2)))
  expect_that( sum(diag(D)), equals(0) )
  expect_that( D[1,2]==D[2,1], is_true() )
  expect_that( D[1,2], equals( 0.1 ) )
})