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
  gst <- Gst_prime( loci, pops,  size.correct=FALSE )
  expect_true( is.na(gst$Gst) )
  expect_that( sum( gst[1,2:4]), is_equivalent_to(0) )
  
  loci <- c( locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,2) ),
             locus( c(2,2) ),
             locus( c(2,2) ) )
  
  gst <- Gst_prime( loci, pops,  size.correct=FALSE )
  expect_that( gst$Gst, is_equivalent_to(1.) )
  expect_that( gst$Hs, is_equivalent_to(0.) )
  expect_that( gst$Ht, is_equivalent_to(0.5) )
  expect_that( gst$P, is_equivalent_to(0) )
  
  loci <- c( locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  
  gst <- Gst_prime( loci, pops,  size.correct=FALSE )
  k <- 2
  hs <- mean( c(He(loci[1:4]), He(loci[5:8])) )
  ht <- He(loci)
  Gst_prime <- (1-hs/ht) * (k-1+hs)
  Gst_prime <- Gst_prime / ((k-1)*(1-hs))
  expect_that( gst$Gst, is_equivalent_to( Gst_prime ) )
  
  loci2 <- c( locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  gst <- Gst_prime(loci2, pops,  size.correct=FALSE )
  hs <- mean( c(He(loci2[1:4]), He(loci2[5:8])) )
  ht <- He(loci2)
  Gst_prime <- (1-hs/ht) * (k-1+hs)
  Gst_prime <- Gst_prime / ((k-1)*(1-hs))
  expect_that( gst$Gst, is_equivalent_to( Gst_prime ) )
  
  df <- data.frame( Population=pops, TPI=loci, PGM=loci2 )
  gst <- Gst_prime( df )
  expect_that( gst, is_a("data.frame") )
  expect_that( dim(gst)[1], is_equivalent_to(3))
  expect_that( dim(gst)[2], is_equivalent_to(5))
  expect_that( names(gst), is_equivalent_to( c("Locus","Gst","Hs","Ht","P")))
  expect_that( gst$Locus, is_equivalent_to( c("TPI","PGM","Multilocus")))
  

})

