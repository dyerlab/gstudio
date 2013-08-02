context("ploidy.R")

test_that("test", {

  
  Z <- locus()
  A <- locus( "A" )
  AA <- locus( c("A","A") )
  AAA <- locus( rep("A",3))
  
  expect_that( ploidy(Z), equals(0) )
  expect_that( ploidy(A), equals(1) )
  expect_that( ploidy(AA), equals(2) )
  expect_that( ploidy(AAA), equals(3) )
  

}
)

