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
  gst <- Gst(pops, loci.fixed )
  expect_that( gst, is_a("data.frame"))
  expect_that( is.na( gst$Gst), is_true() )
  expect_that( is.na( gst$P), is_true() )
  expect_that( gst$Hs, equals(0) )
  expect_that( gst$Ht, equals(0) )
  expect_that( names(gst), is_equivalent_to(c("Gst","Hs","Ht","P")))
    
  loci.fixed <- c( locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(2,2) ),
                   locus( c(2,2) ),
                   locus( c(2,2) ),
                   locus( c(2,2) ) )
  
  gst <- Gst( pops, loci.fixed )
  expect_that( gst$Gst, equals(1.) )
  
  loci <- c( locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  
  gst <- Gst( pops, loci, size.correct=FALSE )
  expect_that( gst$Gst, equals( 1 - 0.375/0.5) )
  
  loci <- c( locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  
  gst <- Gst( pops, loci, size.correct=FALSE )
  expect_that( gst$Gst, equals( 1- (2*.625*.375)/.5) )
  
})