context("dist_cavalli.R")


test_that("individual",{
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  BC <- locus( c("B","C") )
  CD <- locus( c("C","D") )
  
  loci <- as.matrix(c(AA,AB,BB,BC,CD))


})