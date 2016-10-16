context("lag_matrix.R")

test_that("checking",{
  x <- matrix( 1:16, nrow=4, ncol=4)
  x <- x + t(x)
  diag(x) <- 0
  x  
  
  expect_that(lag_matrix(), throws_error())
  expect_that(lag_matrix(x), throws_error())
  expect_that( lag_matrix(x,1), throws_error())
  
  y <- lag_matrix(x, 0, 1 )
  expect_that(sum(y), equals(0) )
  y <- lag_matrix(x,0,10)
  expect_that( sum(y), equals(4) )
  
}
)