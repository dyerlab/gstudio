context("write_population.R")

test_that("test", {

  expect_that( write_population(), throws_error() )
  expect_that( write_population(FALSE), throws_error() )
  
  df <- data.frame()
  expect_that( write_population(df), throws_error() )
  expect_that( write_population(df,file="bob"), throws_error())
})

