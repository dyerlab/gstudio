test_that("is_frequency identifies frequency data frames", {
  expect_false(is_frequency(1:5))
  expect_false(is_frequency(data.frame(x = 1:5)))
  
  df_freq <- data.frame(Allele = c("A", "B"), Frequency = c(0.4, 0.6))
  expect_true(is_frequency(df_freq))
})
