test_that("harmonic_mean calculates correct harmonic mean", {
  x <- c(1, 2, 4)
  # Harmonic mean of 1, 2, 4 is 3 / (1/1 + 1/2 + 1/4) = 3 / 1.75 = 12/7
  expect_equal(harmonic_mean(x), 12 / 7)

  # Drops zero with warning
  expect_warning(hm_zero <- harmonic_mean(c(1, 2, 4, 0)))
  expect_equal(hm_zero, 12 / 7)
})
