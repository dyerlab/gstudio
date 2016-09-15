context("parent_finder.R")

test_that("testing",{
  
  data <- data.frame(ID=1:6)
  data$OffID <- c(0,0,0,1,2,3)
  data$Loc1 <- c( locus(c(1,1)), locus(1:2), locus(c(2,2)), locus(c(1,1)), locus(c(1,3)), locus( c(2,3)))
  

  

  p <- parent_finder( data )  
  expect_that( p, is_a("data.frame"))
  expect_that( nrow(p), equals(6) )
  expect_that( ncol(p), equals(4) )
  expect_that( p$T, is_equivalent_to(c(1,0.5,0.5,0.25,0.5,0.25)))  
  expect_that( p$ParentID, is_equivalent_to(c(1,2,1,2,3,2)))
  expect_that( p$ID, is_equivalent_to(c(4,4,5,5,6,6)))
  expect_that( p$OffID, is_equivalent_to(c(1,1,2,2,3,3)))
  
})