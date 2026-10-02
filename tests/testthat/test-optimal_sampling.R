test_that("optimal_sampling input validation and calculation", {
  expect_error(optimal_sampling())
  expect_error(optimal_sampling(400)) # missing phi

  df <- optimal_sampling(100, 0.25)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("Strata", "Var.Phi", "Var.Phi.Low", "Var.Phi.High") %in% names(df)))
  expect_true(nrow(df) > 0)
  expect_true(all(df$Var.Phi > 0))
  expect_true(all(df$Var.Phi.Low > 0))
  expect_true(all(df$Var.Phi.High > 0))
})
