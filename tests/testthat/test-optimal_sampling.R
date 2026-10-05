test_that("optimal_sampling input validation and calculation", {
  expect_error(optimal_sampling())
  expect_error(optimal_sampling(400)) # missing phi

  df <- optimal_sampling(100, 0.25)
  expect_s3_class(df, "data.frame")
  expect_named(df, c("Stratum", "var_phi", "conf.low", "conf.high"))
  expect_true(nrow(df) > 0)
  expect_true(all(df$var_phi > 0))
  expect_true(all(df$conf.low > 0))
  expect_true(all(df$conf.high > 0))
})
