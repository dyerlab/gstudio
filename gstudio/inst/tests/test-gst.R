context("gst.R")

test_that("checking",{
  loci.fixed <- c( locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ) )
  pops <- factor( rep( c("A","B"), each=4 ) )
  
#  gst.fixed <- gst( pops, loci.fixed )
#  expect_that( gst.fixed, is_a("structure_statistic") )
#  expect_that( is.na(gst.fixed$estimate), is_true() )
  
  
  loci.fixed <- c( locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(2,2) ),
                   locus( c(2,2) ),
                   locus( c(2,2) ),
                   locus( c(2,2) ) )
  
#  gst.fixed <- gst( pops, loci.fixed )
#  expect_that( gst.fixed$estimate, equals(1.) )
  
  loci <- c( locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  
#  gst <- gst( pops, loci, size.correct=FALSE )
#  expect_that( gst$estimate, equals( 1 - 0.375/0.5) )
  
  loci <- c( locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  
#  gst <- Gst( pops, loci, size.correct=FALSE )
#  expect_that( gst$estimate, equals( 1- (2*.625*.375)/.5) )
  
})