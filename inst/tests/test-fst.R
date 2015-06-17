context("fst.R")

test_that("checking",{
  
#   loci <- c( locus( c(1,1) ),
#              locus( c(1,1) ),
#              locus( c(1,1) ),
#              locus( c(1,1) ),
#              locus( c(1,1) ),
#              locus( c(1,1) ),
#              locus( c(1,1) ),
#              locus( c(1,1) ) )
#   pops <- factor( rep( c("A","B"), each=4 ) )
#   df <- data.frame(Population=pops,TPI=loci)
#   
#   fst <- Fst( df )
#   
#   expect_that( gst, is_a("data.frame"))
#   expect_that( fst$Fst, equals(0) )
#   expect_that( gst$sigma2, equals(0) )
#   expect_that( gst$pq, equals(0) )
#   expect_that( names(gst), is_equivalent_to(c("Locus","Fst","sigma2","pq")))
#   
#   loci <- c( locus( c(1,1) ),
#              locus( c(1,1) ),
#              locus( c(1,1) ),
#              locus( c(1,1) ),
#              locus( c(2,2) ),
#              locus( c(2,2) ),
#              locus( c(2,2) ),
#              locus( c(2,2) ) )
#   
#   df$PGM <- loci
#   
#  fst <- Fst( df )
#  expect_that( gst$Gst, equals(1.) )
  
  
  #'  AA <- locus( c("A","A") )
#'  AB <- locus( c("A","B") )
#'  BB <- locus( c("B","B") )
#'  locus <- c(AA,AA,AA,AA,BB,BB,BB,AB,AB,AA)
#'  Population <- c(rep("Pop-A",5),rep("Pop-B",5))
#'  df <- data.frame(Population,locus)
#'  Fst( df )
#'  locus2 <- c(AA,AA,AA,AA,AA,BB,BB,BB,BB,BB)
#'  df <- data.frame( Population, TPI=locus, PGM=locus2 )
#'  Fst( df )
})
