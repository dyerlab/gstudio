test_that("dist_ss input validation", {
  expect_error(dist_ss("not_df"))
  expect_error(dist_ss(data.frame(x = 1:5)))
})

test_that("dist_ss calculates partitioned sum of squares distance", {
  AA <- locus(c("A", "A"))
  BB <- locus(c("B", "B"))
  df <- data.frame(
    Population = c("P1", "P1", "P2", "P2"),
    L = c(AA, AA, BB, BB)
  )

  d <- dist_ss(df)
  expect_true(is.matrix(d))
  expect_equal(dim(d), c(2, 2))
  expect_equal(rownames(d), c("P1", "P2"))
  expect_equal(colnames(d), c("P1", "P2"))
  expect_equal(d[1, 2], d[2, 1])
  expect_true(d[1, 2] > 0)
})
