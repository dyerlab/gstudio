context("hi.R")

test_that("individual heterozygosity",{

  loc1 <- c( locus( c("A","A") ), locus( c("A","B") ), locus( c("A","B") ) )
  loc2 <- c( locus( c("A","B") ), locus( c("A","A") ), locus( c("A","B") ) )
  loc3 <- c( locus( c("A","B") ), locus( c("A","A") ), locus( c("A","B") ) )
  loc4 <- c( locus( c("A","A") ), locus( c("A","A") ), NA )
  Population <- c("A","A","B")
  df <- data.frame( ID = 1:3, Population, loc1, loc2, loc3, loc4 )

  
  hi <- Hi(df)
  
  expect_that( is(hi,"data.frame"), equals(TRUE))
  expect_that( hi$Hi, is_equivalent_to( c(0.5, 0.25, 1.0 ) ) )

})