context("gst.R")

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
  gst <- Gst( loci, pops )
  
  expect_that( gst, is_a("data.frame"))
  expect_that( is.na( gst$Gst), is_true() )
  expect_that( is.na( gst$P), is_true() )
  expect_that( gst$Hs, equals(0) )
  expect_that( gst$Ht, equals(0) )
  expect_that( names(gst), is_equivalent_to(c("Gst","Hs","Ht","P")))
    
  loci <- c( locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(2,2) ),
                   locus( c(2,2) ),
                   locus( c(2,2) ),
                   locus( c(2,2) ) )
  
  gst <- Gst( loci, pops )
  expect_that( gst$Gst, equals(1.) )
  
  loci.1 <- c( locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  
  gst <- Gst( loci.1, pops, size.correct=FALSE )
  expect_that( gst$Gst, equals( 1 - 0.375/0.5) )
  
  loci.2 <- c( locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  
  gst <- Gst( loci.2, pops, size.correct=FALSE )
  expect_that( gst$Gst, equals( 1- (2*.625*.375)/.5) )
  
  
  loci <- data.frame( Population=pops, TPI = loci.1, PGM=loci.2)
  gst <- Gst( loci, nperm=99 )
  expect_that( gst, is_a("data.frame") )
  
  
  expect_that( dim(gst)[1], equals(3))
  expect_that( dim(gst)[2], equals(5))
  expect_that( names(gst), is_equivalent_to( c("Locus","Gst","Hs","Ht","P")))
  expect_that( gst$Locus, is_equivalent_to( c("TPI","PGM","Multilocus")))
  
})