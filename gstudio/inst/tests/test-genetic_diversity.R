context("genetic_diversity.R")


test_that("error checks",{

  
  expect_that( genetic_diversity( data.frame(X=1), mode="Bob"), throws_error() )
  expect_that( genetic_diversity( numeric(10), mode="Ae"), throws_error() )
  expect_that( genetic_diversity( data.frame(A=numeric(0),mode="Ae")), throws_error() )
  
  
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
  
  
  gd <- genetic_diversity( df )
  expect_that( gd, is_a("data.frame"))
  expect_that( dim(gd), is_equivalent_to(c(1,2)))
  expect_that( names(gd), is_equivalent_to(c("Locus","Ae")))
  expect_that( gd[1,2], is_equivalent_to( 4 ))
  
  gd <- genetic_diversity( df, stratum="Population")
  expect_that( dim(gd), is_equivalent_to(c(3,3)))
  expect_that( names(gd), is_equivalent_to(c("Stratum","Locus","Ae")))
    
})


