context("is_na.R")

test_that("locus",{
  loc1 <- locus()
  loc2 <- locus( 1:2 )
  
  expect_that( is.na(loc1), is_true() )
  expect_that( is.na(loc2), is_false() )
})

