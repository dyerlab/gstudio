context("dist_jaccard.R")


test_that("individual",{

  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  AC <- locus( c("A","C") )
  BB <- locus( c("B","B") )
  BC <- locus( c("B","C") )
  CC <- locus( c("C","C") )
  loci <- c(AA,AA,AB,AA,BB,BC,CC,BB,BB,CC)
  df <- data.frame( Population=c(rep("A",5),rep("B",5) ), TPI=loci )

  
  expect_that( dist_jaccard("Bob"), throws_error() )
  expect_that( dist_jaccard(data.frame(Population="A")), throws_error() )
  
  D <- dist_jaccard(df)
  expect_that( D, is_a("matrix") )
  expect_that( dim(D), is_equivalent_to(c(2,2)))
  expect_that( sum(diag(D)), equals(0) )
  expect_that( D[1,2]==D[2,1], is_true() )
  expect_that( D[1,2], equals( (2*.1)/(1+.1) ) )
  
})