test_that("maximum_allele_size finds max digit length across loci", {
  expect_error(maximum_allele_size())
  expect_error(maximum_allele_size("not_locus"))

  loc1 <- c(locus(c(1, 12)), locus(c(2, 22)))
  expect_equal(maximum_allele_size(loc1), 2)

  loc2 <- c(locus(c(101, 102)), locus(c(1001, 1002)))
  expect_equal(maximum_allele_size(loc2), 4)

  # Data.frame test
  df <- data.frame(L1 = loc1, L2 = loc2)
  expect_equal(maximum_allele_size(df), 4)
})
