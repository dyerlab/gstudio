context("is_na.R")

test_that("locus",{
  loc1 <- locus()
  loc2 <- locus( 1:2 )
  
  expect_that( is.na(loc1), is_true() )
  expect_that( is.na(loc2), is_false() )
})

test_that( "structure_statistic", {
  
#   ss1 <- structure_statistic("test1",NA )
#   ss2 <- structure_statistic( "test2", 0 )
#   
#   expect_that( is.na(ss1), is_true() )
#   expect_that( is.na(ss2), is_false() )
  
})

