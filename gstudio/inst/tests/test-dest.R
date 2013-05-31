context("Dest.R")
 
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
  
  est <- Dest( loci.fixed, pops )
  expect_that( est$Dest, equals(0) )
  
  
  loci.fixed <- c( locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(1,1) ),
                   locus( c(2,2) ),
                   locus( c(2,2) ),
                   locus( c(2,2) ),
                   locus( c(2,2) ) )
  
  est <- Dest( loci.fixed, pops, size.correct=FALSE )
  expect_that( est$Dest, equals(0.25) )
  
  loci <- c( locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  
  est <- Dest( loci, pops,  size.correct=FALSE )
  expect_that( est$Dest, equals( ((0.5-0.375) / (1-0.375) / 2 ) ) )
  
  loci2 <- c( locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  
  est <- Dest( loci2,pops,  size.correct=FALSE )
  expect_that( est$Dest, equals( (0.5-0.46875) / (1-0.46875) / 2 ) )
  
  df <- data.frame(Population=pops, TPI=loci, PGM=loci2 )
  est <- Dest( df )
  expect_that( est, is_a("data.frame") )
  expect_that( dim(est)[1], equals(3))
  expect_that( dim(est)[2], equals(5))
  expect_that( names(est), is_equivalent_to( c("Locus","Dest","Hs","Ht","P")))
  expect_that( est$Locus, is_equivalent_to( c("TPI","PGM","Multilocus")))
  
})