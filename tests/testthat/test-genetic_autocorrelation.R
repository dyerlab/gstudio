test_that("genetic_autocorrelation input validation", {
  expect_error(genetic_autocorrelation())
  expect_error(genetic_autocorrelation(matrix(1:4, 2, 2)))
  expect_error(genetic_autocorrelation(matrix(1:4, 2, 2), matrix(1:9, 3, 3), bins = c(0, 1)))
})

test_that("genetic_autocorrelation calculates autocorrelation and permutations", {
  set.seed(42)
  # 10 individuals on a 1D line
  coords <- matrix(seq(1, 10, by = 1), ncol = 1)
  P <- as.matrix(dist(coords))
  
  # Create synthetic genetic distance correlated with physical distance
  G <- P + matrix(rnorm(100, sd = 0.1), 10, 10)
  G <- (G + t(G)) / 2
  diag(G) <- 0

  bins <- c(0, 2, 5, 10)
  res <- genetic_autocorrelation(P, G, bins, perms = 0)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  expect_equal(names(res), c("From", "To", "R", "N", "p.value"))
  expect_equal(res$From, c(0, 2, 5))
  expect_equal(res$To, c(2, 5, 10))
  expect_true(all(is.na(res$p.value)))
  expect_true(all(res$N > 0))

  # Permutation test
  res_perm <- genetic_autocorrelation(P, G, bins, perms = 19)
  expect_true(all(!is.na(res_perm$p.value)))
  expect_true(all(res_perm$p.value > 0 & res_perm$p.value <= 1))
})
