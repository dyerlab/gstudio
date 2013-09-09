context("geom_locus.R")

test_that("shortest paths", {

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
  TPI <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD)
  
  df <- data.frame( strata = c(rep("A",5),rep("B",5)), TPI  )
  p <- geom_locus( aes(x=TPI), data=df )
  expect_that( p, is_a("proto"))
  expect_that( p, is_a("environment"))
  
  
  p <- geom_locus( aes(x=TPI, fill=strata), data=df )
  expect_that( p, is_a("proto"))
  expect_that( p, is_a("environment"))
  
  
})