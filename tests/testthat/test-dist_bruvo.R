# Tests for dist_bruvo() and genetic_distance(mode = "bruvo").

test_that("diploid distances follow 1 - 2^-x over the best allele pairing", {
  loci <- c(locus(c("150", "152")), locus(c("150", "154")), locus(c("152", "150")), locus(c("170", "170")))
  D <- dist_bruvo(loci, repeat_length = 2)
  expect_true(is.matrix(D))
  expect_equal(dim(D), c(4, 4))
  expect_equal(diag(D), rep(0, 4))
  expect_equal(D, t(D))
  # 75/76 vs 75/77: pairing (75-75, 76-77) = (0 + 0.5) / 2 beats (75-77, 76-75)
  expect_equal(D[1, 2], 0.25)
  # allele order does not matter
  expect_equal(D[1, 3], 0)
  # 75/76 vs 85/85: (1 - 2^-10 + 1 - 2^-9) / 2
  expect_equal(D[1, 4], (2 - 2^-10 - 2^-9) / 2)
  expect_true(all(D >= 0 & D <= 1))
})

test_that("haploid and tetraploid loci work and match a brute-force pairing", {
  H <- dist_bruvo(c(locus("140"), locus("146")), repeat_length = 2)
  expect_equal(H[1, 2], 1 - 2^-3)

  a <- c(10, 12, 13, 20); b <- c(11, 13, 19, 25)
  T4 <- dist_bruvo(c(locus(as.character(a)), locus(as.character(b))))
  perms <- .permutations(4)
  expect_length(perms, 24)
  brute <- min(vapply(perms, function(p) mean(1 - 2^-abs(a - b[p])), numeric(1)))
  expect_equal(T4[1, 2], brute)
})

test_that("multilocus distance averages over loci typed in both individuals", {
  df <- data.frame(L1 = c(locus(c("10", "10")), locus(c("10", "11")), locus()),
                   L2 = c(locus(c("20", "20")), locus(c("22", "22")), locus(c("20", "21"))))
  D <- dist_bruvo(df)
  expect_equal(D[1, 2], mean(c(0.25, 0.75)))
  expect_equal(D[1, 3], 0.25)          # L1 missing in 3: L2 only
  expect_equal(D[2, 3], (0.75 + 0.5) / 2)

  # different ploidy at the only locus: no comparison
  M <- dist_bruvo(c(locus("10"), locus(c("10", "12"))))
  expect_true(is.na(M[1, 2]))
})

test_that("repeat_length takes one value or one per locus", {
  df <- data.frame(L1 = c(locus(c("100", "100")), locus(c("102", "102"))),
                   L2 = c(locus(c("100", "100")), locus(c("103", "103"))))
  D <- dist_bruvo(df, repeat_length = c(L2 = 3, L1 = 2))
  expect_equal(D[1, 2], 0.5)
  expect_error(dist_bruvo(df, repeat_length = c(L1 = 2)), "L2")
  expect_error(dist_bruvo(df, repeat_length = 0), "positive")
  expect_error(dist_bruvo(c(locus(c("A", "B")), locus(c("A", "A")))), "numeric allele sizes")
  expect_error(dist_bruvo("Bob"), "locus vector")
})

test_that("genetic_distance(mode = 'bruvo') passes repeat_length through", {
  data(cornus_florida, package = "gstudio", envir = environment())
  x <- cornus_florida[1:6, ]
  D <- genetic_distance(x, mode = "bruvo", repeat_length = 2)
  expect_equal(D, dist_bruvo(x, repeat_length = 2))
  expect_false(isTRUE(all.equal(D, genetic_distance(x, mode = "Bruvo"))))
  expect_warning(genetic_distance(x, mode = "amova", repeat_length = 2), "repeat_length")
})
