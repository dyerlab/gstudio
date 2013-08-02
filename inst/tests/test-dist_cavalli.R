context("dist_cavalli.R")


test_that("individual",{
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
  df <- data.frame( Population=c(rep("Pop-A",5),rep("Pop-B",5)), TPI=loc1, PGM=loc2 )
  
  suppressWarnings( Dc <- dist_cavalli( df ) )
  expect_that( Dc, is_a("matrix"))
  expect_that( dim(Dc), is_equivalent_to( c(2,2) ) )
  expect_that( sum(diag(Dc)), equals(0))
  expect_that( Dc[1,2]==Dc[2,1], is_true() )
  
  cat("?")
  expect_that( Dc[1,2]>0.73, is_true() )
  expect_that( Dc[1,2]<0.74, is_true() )

})