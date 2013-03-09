context("genetic_distance.R")


test_that("error checks",{

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
  loc1 <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD)
  loc2 <- c(AA,AA,AC,AA,CC,CC,AC,CC,AA,AC)
  df <- data.frame( Population=c(rep("A",5),rep("B",5)), TPI=loc1, PGM=loc2 )
  
  expect_that( genetic_distance(df, mode="Bob"), throws_error() )
  expect_that( genetic_distance( numeric(10), mode="AMOVA"), throws_error() )
  expect_that( genteic_distance( data.frame(A=numeric(0),mode="AMOVA")), throws_error() )
  
})


