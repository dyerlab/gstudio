
test_that("test",{
  
  df <- data.frame( Int=1:10, Num=runif(n=10), Log=sample(c(TRUE,FALSE),size=10,replace=TRUE) )
  
  expect_that( column_class(df,"integer"), equals("Int"))
  expect_that( column_class(df,"integer",mode="index"), equals(1) )
  
  expect_that( column_class(df,"numeric",mode="label"), equals("Num"))
  expect_that( column_class(df,"numeric",mode="index"), equals(2) )
  
  expect_that( column_class(df,"logical"), equals("Log"))
  expect_that( column_class(df,"logical",mode="index"), equals(3) )
  
  expect_identical( column_class(df,"locus"), character(0) )
  expect_identical( column_class(df,"locus",mode="index"), integer(0) )

  # Tibble compatibility (tbl_df subclass)
  df_tibble <- df
  df_tibble$loc <- locus(1:2)
  class(df_tibble) <- c("tbl_df", "tbl", "data.frame")
  expect_equal( column_class(df_tibble, "locus"), "loc" )
  expect_equal( column_class(df_tibble, "locus", mode = "index"), 4 )
})