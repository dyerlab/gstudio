context("ht.R")

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
   
   ht <- Ht( df )
   
   expect_that( ht, is_a("data.frame"))
   expect_that( ht$Ht, equals(0) )
   expect_that( names(ht), is_equivalent_to(c("Locus","Ht")))
   
   loci <- c( locus( c(1,1) ),
              locus( c(1,1) ),
              locus( c(1,1) ),
              locus( c(1,1) ),
              locus( c(2,2) ),
              locus( c(2,2) ),
              locus( c(2,2) ),
              locus( c(2,2) ) )
   
   df$PGM <- loci
   
  ht <- Ht( df )
  expect_that( ht$Ht, is_equivalent_to(c(0,0.5) ) ) 
  
  
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  locus <- c(AA,AA,AA,AA,BB,BB,BB,AB,AB,AA)
  Population <- c(rep("Pop-A",5),rep("Pop-B",5))
  df <- data.frame(Population,locus)
  ht <- Ht( df )
  expect_that( ht$Ht, is_equivalent_to(0.5175) )
  

})
