test_that("dist2cov and distance_to_covariance match textbook Gower centering", {
  D <- matrix(c(0, 7, 6, 7,
                7, 0, 13, 8,
                6, 13, 0, 4,
                7, 8, 4, 0), nrow = 4, byrow = TRUE)

  n <- nrow(D)
  H <- diag(n) - matrix(1 / n, n, n)
  expected_C <- H %*% (-0.5 * D) %*% H

  c1 <- dist2cov(D)
  c2 <- distance_to_covariance(D)

  expect_equal(c1, expected_C)
  expect_equal(c2, expected_C)
  expect_equal(c1, c2)

  # Inversion: converting covariance back to distance restores D
  d_back <- cov2dist(c1)
  expect_equal(d_back, D)
})
