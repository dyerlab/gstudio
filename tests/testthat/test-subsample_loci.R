test_that("subsample_loci samples correct number of loci", {
  data(arapat)
  total_loci <- length(column_class(arapat, "locus"))

  expect_error(subsample_loci(arapat, numLoci = total_loci + 1))
  expect_error(subsample_loci(arapat, numLoci = total_loci))

  sub <- subsample_loci(arapat, numLoci = 3)
  expect_s3_class(sub, "data.frame")
  expect_equal(length(column_class(sub, "locus")), 3)
  expect_true(all(c("Species", "Population") %in% names(sub)))
})
