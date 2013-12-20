context("multilocus_diversity.R")

test_that("checking",{
  
  loci <- c( locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ) )
  pops <- factor( rep( c("A","B"), each=4 ) )
  df <- data.frame( pops, loci )
  r <- multilocus_diversity( df )
  expect_that( r, equals( 1/8))
  
  
  df$loc2 <- c( locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(2,2) ),
                   locus( c(2,2) ),
                   locus( c(2,2) ),
                   locus( c(2,2) ) )
  r <- multilocus_diversity( df )
  expect_that( r, equals( 2/8))

  df$loc3 <-c( locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  
  r <- multilocus_diversity( df )
  expect_that( r, equals( 4/8))
  
})