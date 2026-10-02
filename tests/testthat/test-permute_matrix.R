test_that("permute_matrix preserves dimensions and elements", {
  m <- matrix(1:16, nrow = 4)
  p <- permute_matrix(m)

  expect_true(is.matrix(p))
  expect_equal(dim(p), c(4, 4))
  expect_equal(sort(as.vector(p)), sort(as.vector(m)))
})
