context("dist_euclidean.R")


test_that("individual",{
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  loci <- c(AA,AA,AB,AA,BB,BB,BB,AB,BB,AB)
  df <- data.frame( Population=c(rep("A",5),rep("B",5) ), TPI=loci )
  
  
  expect_that( dist_euclidean("Bob"), throws_error() )
  expect_that( dist_euclidean(data.frame(Population="A")), throws_error() )
  
  De <- dist_euclidean(df)
  expect_that( De, is_a("matrix") )
  expect_that( dim(De), is_equivalent_to(c(2,2)))
  expect_that( sum(diag(De)), equals(0) )
  expect_that( De[1,2]==De[2,1], is_true() )
  expect_that( De[1,2], equals( sqrt(0.5 ) ) )
  
               
               
})