context("gst_prime.R")


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
  gst <- Gst_prime( pops, loci, size.correct=FALSE )
  expect_that( is.na(gst$Gst), is_true() )
  expect_that( sum( gst[1,2:4]), equals(0) )
  
  loci <- c( locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,2) ),
             locus( c(2,2) ),
             locus( c(2,2) ) )
  
  gst <- Gst_prime( pops, loci, size.correct=FALSE )
  expect_that( gst$Gst, equals(1.) )
  expect_that( gst$Hs, equals(0.) )
  expect_that( gst$Ht, equals(0.5) )
  expect_that( gst$P, equals(0) )
  
  loci <- c( locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  
  gst <- Gst_prime( pops, loci, size.correct=FALSE )
  k <- 2
  hs <- mean( c(He(loci[1:4]), He(loci[5:8])) )
  ht <- He(loci)
  Gst_prime <- (1-hs/ht) * (k-1+hs)
  Gst_prime <- Gst_prime / ((k-1)*(1-hs))
  expect_that( gst$Gst, equals( Gst_prime ) )
  
  loci <- c( locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  gst <- Gst_prime( pops, loci, size.correct=FALSE )
  hs <- mean( c(He(loci[1:4]), He(loci[5:8])) )
  ht <- He(loci)
  Gst_prime <- (1-hs/ht) * (k-1+hs)
  Gst_prime <- Gst_prime / ((k-1)*(1-hs))
  
  expect_that( gst$Gst, equals( Gst_prime ) )
  
})

