context("genetic_distance.R")


test_that("error checks",{

  
  expect_that( genetic_distance( data.frame(X=1), mode="Bob"), throws_error() )
  expect_that( genetic_distance( numeric(10), mode="AMOVA"), throws_error() )
  expect_that( genteic_distance( data.frame(A=numeric(0),mode="AMOVA")), throws_error() )
  
  
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  AC <- locus( c("A","C") )
  AD <- locus( c("A","D") )
  BC <- locus( c("B","C") )
  BD <- locus( c("B","D") )
  CC <- locus( c("C","C") )
  CD <- locus( c("C","D") )
  DD <- locus( c("D","D") )
  loci <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD) 
  stratum <- c(rep("A",3), rep("B",3), rep("C",4) )
  df <- data.frame( Population=stratum, Locus=loci)
  
  expect_that( all(dist_amova( loci ) == genetic_distance(loci, mode="amova")), is_true() )
  
  expect_that( all(dist_jaccard(df) == genetic_distance(loci, stratum, "jaccard")), is_true() ) 
  
  
})


