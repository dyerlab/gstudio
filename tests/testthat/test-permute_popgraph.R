test_that("permute_popgraph input validation and edge bootstrapping", {
  expect_error(permute_popgraph("not_matrix", factor(1)))
  expect_error(permute_popgraph(matrix(1:4, 2, 2), factor(1))) # mismatched size

  data(arapat)
  mv <- to_mv(arapat)
  groups <- arapat$Population

  # Fast run with nboot = 5
  suppressMessages(
    boot_g <- permute_popgraph(mv, groups, nboot = 5)
  )
  expect_true(inherits(boot_g, "popgraph") || inherits(boot_g, "igraph"))
  expect_true(igraph::ecount(boot_g) > 0)
})
