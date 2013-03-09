context("column_class.R")

test_that("test",{
  
  df <- data.frame( Int=1:10, Num=runif(n=10), Log=sample(c(TRUE,FALSE),size=10,replace=TRUE) )
  
  expect_that( column_class(df,"integer"), equals("Int"))
  expect_that( column_class(df,"integer",mode="index"), equals(1) )
  
  expect_that( column_class(df,"numeric",mode="label"), equals("Num"))
  expect_that( column_class(df,"numeric",mode="index"), equals(2) )
  
  expect_that( column_class(df,"logical"), equals("Log"))
  expect_that( column_class(df,"logical",mode="index"), equals(3) )
  
})