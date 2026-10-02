test_that("allele_counts input validation", {
  expect_error(allele_counts("not_df"))
  expect_error(allele_counts(data.frame(x = 1), locus = "missing"))
  expect_error(allele_counts(data.frame(Population = 1, L1 = locus(1:2)), locus = "L1", stratum = "missing"))
})

test_that("allele_counts computes correct counts by stratum", {
  AA <- locus(c("A", "A"))
  AB <- locus(c("A", "B"))
  BB <- locus(c("B", "B"))

  # Pop 1: 2 AA, 1 AB -> 5 A, 1 B
  # Pop 2: 3 BB       -> 0 A, 6 B
  df <- data.frame(
    Population = c(rep("P1", 3), rep("P2", 3)),
    LOC = c(AA, AA, AB, BB, BB, BB)
  )

  cts <- allele_counts(df, locus = "LOC", stratum = "Population")
  expect_s3_class(cts, "data.frame")
  expect_equal(cts$Stratum, c("P1", "P2"))
  expect_equal(cts$A, c(5, 0))
  expect_equal(cts$B, c(1, 6))
})

test_that("allele_counts handles missing genotypes", {
  AA <- locus(c("A", "A"))
  df <- data.frame(
    Population = c("P1", "P1", "P2"),
    LOC = c(AA, locus(), locus())
  )
  cts <- allele_counts(df, locus = "LOC", stratum = "Population")
  expect_equal(cts$Stratum, c("P1", "P2"))
  expect_equal(cts$A, c(2, 0))
})
