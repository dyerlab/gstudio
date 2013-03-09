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
  k <- length(levels(pops))
  
#  gst <- Gst_prime( pops, loci )
#  expect_that( gst, is_a("structure_statistic") )
#  expect_that( is.na(gst), is_true() )
  
  
  loci <- c( locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,2) ),
             locus( c(2,2) ),
             locus( c(2,2) ) )
  
#  gst <- Gst_prime( pops, loci, size.correct=FALSE )
#  expect_that( gst$estimate, equals(1.) )
  
  loci <- c( locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  
#  gst <- Gst_prime( pops, loci, size.correct=FALSE )
#  hs <- mean( c(he(allele.spectrum(loci[1:4])), he(allele.spectrum(loci[5:8]))))
#  ht <- he(allele.spectrum(loci))
#  expect_that( gst$estimate, equals( (1 - hs/ht * (k-1+hs) ) / ((k-1)*(1-hs)) ) )
  
  loci <- c( locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  
#  gst <- Gst_prime( pops, loci, size.correct=FALSE )
#  hs <- mean( c(he(allele.spectrum(loci[1:4])), he(allele.spectrum(loci[5:8]))))
#  ht <- he(allele.spectrum(loci))
#  expect_that( gst$estimate, equals( (1 - hs/ht * (k-1+hs) ) / ((k-1)*(1-hs)) ) )
  
})

