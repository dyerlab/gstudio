context("is_na.R")

test_that("locus",{
  loc1 <- locus()
  loc2 <- locus( 1:2 )
  
  expect_true( is.na(loc1) )
  expect_false( is.na(loc2) )
})

