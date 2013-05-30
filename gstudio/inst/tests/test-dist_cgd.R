context("dist_cgd.R")


test_that("test",{

  expect_that( dist_cgd("Bob"), throws_error() )
  expect_that( dist_cgd(data.frame(Pop=1)), throws_error() )
  expect_that( dist_cgd(data.frame(Pop=1), stratum="bob"), throws_error() )
  
})