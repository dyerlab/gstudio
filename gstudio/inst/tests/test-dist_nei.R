context("dist_nei.R")


test_that("test",{

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
  
  d <- dist_nei( df )
  
  expect_that( d, is_a("matrix"))
  expect_that( nrow(d), equals(2) )
  expect_that( ncol(d), equals(2) )
  expect_that( sum(diag(d)), equals(0))
  
  expect_that( d[1,2]==d[2,1], is_true() )
  expect_that( (d[1,2]-0.3507115 < 2e-7), is_true() )
  
  

})