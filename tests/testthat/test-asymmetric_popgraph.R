context("asymmetric_popgraph.R")

make_test_graph <- function() {
  A <- matrix(0, nrow = 4, ncol = 4)
  A[1, 2] <- 0.5
  A[1, 3] <- 1.2
  A[2, 3] <- 0.8
  A[3, 4] <- 0.4
  A <- A + t(A)
  rownames(A) <- colnames(A) <- LETTERS[1:4]
  g <- igraph::graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE, diag = FALSE)
  igraph::V(g)$label <- paste0("pop_", LETTERS[1:4])
  g
}

test_that("input validation", {
  g <- make_test_graph()

  expect_error(asymmetric_popgraph("not a graph"))
  expect_error(asymmetric_popgraph(igraph::as_directed(g)))
})

test_that("returns a directed popgraph", {
  g <- make_test_graph()
  result <- asymmetric_popgraph(g)

  expect_s3_class(result, "igraph")
  expect_s3_class(result, "popgraph")
  expect_true(igraph::is_directed(result))
})

test_that("directed graph has 2x edges (one per direction per undirected edge)", {
  g <- make_test_graph()
  result <- asymmetric_popgraph(g)

  expect_equal(igraph::ecount(result), 2 * igraph::ecount(g))
})

test_that("vertex count and names are preserved", {
  g <- make_test_graph()
  result <- asymmetric_popgraph(g)

  expect_equal(igraph::vcount(result), igraph::vcount(g))
  expect_setequal(igraph::V(result)$name, igraph::V(g)$name)
})

test_that("vertex attributes are carried over", {
  g <- make_test_graph()
  result <- asymmetric_popgraph(g)

  expect_equal(sort(igraph::V(result)$label), sort(igraph::V(g)$label))
})

test_that("edge weights sum to twice the total undirected cGD", {
  g <- make_test_graph()
  result <- asymmetric_popgraph(g)
  w <- asymmetric_weights(g)

  expect_equal(sum(igraph::E(result)$weight), sum(w$cGD), tolerance = 1e-10)
})

test_that("all edge weights are positive", {
  g <- make_test_graph()
  result <- asymmetric_popgraph(g)

  expect_true(all(igraph::E(result)$weight > 0))
})
