context("indices.R")

test_that("indices",{
  N <- 4
  idx <- indices(1:N, FALSE)
  
  expect_that( idx, is_a("matrix"))
  expect_that( dim(idx), is_equivalent_to(c(16,2) ) )
  expect_that( idx, is_equivalent_to( cbind( rep(1:N, each=4), rep(1:N,times=4) ) ) )
  
  idx.sm <- indices(1:N)
  expect_that( dim(idx.sm), is_equivalent_to( c(6,2) ) )
})