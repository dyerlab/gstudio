
test_that("locus",{
  loc1 <- locus()
  loc2 <- locus( 1:2 )
  loc3 <- locus( NA )
  loc4 <- locus( c(NA, NA) )
  loc5 <- locus( "NA", type = "separated" )
  
  expect_true( is.na(loc1) )
  expect_false( is.na(loc2) )
  expect_true( is.na(loc3) )
  expect_true( is.na(loc4) )
  expect_true( is.na(loc5) )

  # Vectorized test
  vec <- c(loc1, loc2, loc3, loc4, loc5)
  expect_equal( is.na(vec), c(TRUE, FALSE, TRUE, TRUE, TRUE) )
})

