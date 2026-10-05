test_that("dist_pgd validates input", {
  expect_error(dist_pgd("Bob"))
  expect_error(dist_pgd(data.frame(Pop = 1)))
  expect_error(dist_pgd(data.frame(Pop = 1), stratum = "bob"))
})

test_that("genetic_distance(mode = 'pgd') returns directed partitioned cGD", {
  data(gravity, package = "gstudio", envir = environment())
  P <- genetic_distance(gravity, mode = "pgd")
  graph <- popgraph(to_mv(gravity), factor(as.character(gravity$Population)))

  expect_equal(dim(P), c(25L, 25L))
  expect_equal(rownames(P), colnames(P))
  expect_equal(P, pgd(graph, output = "matrix"))
  expect_equal(unname(diag(P)), rep(0, 25))
  expect_false(isSymmetric(unname(P)))                  # direction matters

  # each direct edge's two shares sum to its cGD
  el <- igraph::as_edgelist(graph, names = TRUE)
  arcs <- pgd(graph, output = "graph")
  A <- igraph::as_adjacency_matrix(arcs, attr = "weight", sparse = FALSE)
  expect_equal(A[el] + A[el[, 2:1]], igraph::E(graph)$weight, tolerance = 1e-12)

  # shorter downstream: gene flow runs toward higher-numbered populations
  up <- outer(seq_len(25), seq_len(25), "<")
  expect_lt(mean(P[up]), mean(t(P)[up]))

  # never longer than the symmetric cGD path it partitions
  C <- genetic_distance(gravity, mode = "cgd")
  expect_true(all(pmin(P, t(P)) <= C + 1e-9))

  # gamma is passed through; stray arguments to other modes are reported
  expect_equal(genetic_distance(gravity, mode = "pgd", gamma = 1), pgd(graph, gamma = 1, output = "matrix"))
  expect_warning(genetic_distance(gravity, mode = "nei", gamma = 1), "gamma")
})
