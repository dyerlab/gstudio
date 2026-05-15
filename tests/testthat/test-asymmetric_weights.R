context("asymmetric_weights.R")

# Minimal weighted undirected popgraph shared across tests:
#   A - B - C - D with a shortcut A - C
#   Weights chosen to be asymmetric across nodes so pij != pji.
make_test_graph <- function() {
  A <- matrix(0, nrow = 4, ncol = 4)
  A[1, 2] <- 0.5
  A[1, 3] <- 1.2
  A[2, 3] <- 0.8
  A[3, 4] <- 0.4
  A <- A + t(A)
  rownames(A) <- colnames(A) <- LETTERS[1:4]
  igraph::graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE, diag = FALSE)
}

test_that("input validation", {
  g <- make_test_graph()

  expect_error(asymmetric_weights("not a graph"))
  expect_error(asymmetric_weights(igraph::as_directed(g)))
})

test_that("returns a data.frame with correct columns", {
  g <- make_test_graph()
  result <- asymmetric_weights(g)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("i", "j", "pij", "pji", "cGD", "dij", "dji", "Delta"))
})

test_that("one row per undirected edge", {
  g <- make_test_graph()
  result <- asymmetric_weights(g)

  expect_equal(nrow(result), igraph::ecount(g))
})

test_that("dij + dji == cGD for every edge", {
  g <- make_test_graph()
  result <- asymmetric_weights(g)

  expect_equal(result$dij + result$dji, result$cGD, tolerance = 1e-10)
})

test_that("Delta == dij - dji", {
  g <- make_test_graph()
  result <- asymmetric_weights(g)

  expect_equal(result$Delta, result$dij - result$dji, tolerance = 1e-10)
})

test_that("pij and pji are probabilities in (0, 1]", {
  g <- make_test_graph()
  result <- asymmetric_weights(g)

  expect_true(all(result$pij > 0 & result$pij <= 1))
  expect_true(all(result$pji > 0 & result$pji <= 1))
})

test_that("total graph distance is preserved", {
  g <- make_test_graph()
  result <- asymmetric_weights(g)

  original_total <- sum(igraph::E(g)$weight)
  expect_equal(sum(result$cGD),  original_total, tolerance = 1e-10)
  expect_equal(sum(result$dij) + sum(result$dji), original_total, tolerance = 1e-10)
})

test_that("node names in output match graph vertex names", {
  g <- make_test_graph()
  result <- asymmetric_weights(g)
  node_names <- igraph::V(g)$name

  expect_true(all(result$i %in% node_names))
  expect_true(all(result$j %in% node_names))
})
