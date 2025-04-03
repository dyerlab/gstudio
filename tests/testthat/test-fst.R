context("fst.R")

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
  df <- data.frame(Population=pops,TPI=loci)
  
  fst <- Fst( df )
  
  expect_that( fst, is_a("data.frame"))
  expect_that( fst$Fst, is.nan )
  
  loci <- c( locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,2) ),
             locus( c(2,2) ),
             locus( c(2,2) ) )
  
  df$PGM <- loci
  
  fst <- Fst( df )
  expect_that( fst$Fst, equals(c(1.0, NaN)) )
  
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  locus <- c(AA,AA,AA,AA,BB,BB,BB,AB,AB,AA)
  Population <- c(rep("Pop-A",5),rep("Pop-B",5))
  df <- data.frame(Population,locus)
  fst <- Fst( df )
  
  expect_that( fst$Hs, equals(0.475) )
  expect_that( fst$Ht, equals(0.5175) )
  
  locus2 <- c(AA,AA,AA,AA,AA,BB,BB,BB,BB,BB)
  df <- data.frame( Population, TPI=locus, PGM=locus2 )
  fst <- Fst( df )
  
  expect_that( fst$Hs, equals( c(0, 0.475)))
  expect_that( fst$Ht, equals( c(0.5, 0.5175)))
  
})
