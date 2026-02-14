context("geom_popgraph.R")

# Helper: create a small test graph
make_test_graph <- function() {
  a <- matrix(c(0,1,0,1, 1,0,0,1, 0,0,0,1, 1,1,1,0), nrow = 4)
  rownames(a) <- colnames(a) <- LETTERS[1:4]
  g <- as.popgraph(a)
  igraph::V(g)$x <- c(1, 2, 3, 4)
  igraph::V(g)$y <- c(10, 20, 30, 40)
  igraph::V(g)$group <- c("pop1", "pop1", "pop2", "pop2")
  g
}

test_that("returns a list of two ggplot layers", {
  g <- make_test_graph()
  layers <- geom_popgraph(g, aes(x = x, y = y))
  expect_is(layers, "list")
  expect_equal(length(layers), 2)
  expect_true(inherits(layers[[1]], "LayerInstance"))
  expect_true(inherits(layers[[2]], "LayerInstance"))
})

test_that("coordinates extracted from vertex attributes", {
  g <- make_test_graph()
  layers <- geom_popgraph(g, aes(x = x, y = y))
  # Node layer is second; check its data has the right coordinates
  node_data <- layers[[2]]$data
  expect_equal(node_data$x, c(1, 2, 3, 4))
  expect_equal(node_data$y, c(10, 20, 30, 40))
})

test_that("auto-layout when no x/y mapping provided", {
  g <- make_test_graph()
  layers <- geom_popgraph(g)
  node_data <- layers[[2]]$data
  expect_true("x" %in% names(node_data))
  expect_true("y" %in% names(node_data))
  expect_equal(nrow(node_data), 4)
})

test_that("fill aesthetic maps a vertex attribute", {
  g <- make_test_graph()
  layers <- geom_popgraph(g, aes(x = x, y = y, fill = group))
  node_data <- layers[[2]]$data
  expect_true("group" %in% names(node_data))
  expect_equal(node_data$group, c("pop1", "pop1", "pop2", "pop2"))
})

test_that("edge data.frame has correct dimensions", {
  g <- make_test_graph()
  layers <- geom_popgraph(g, aes(x = x, y = y))
  edge_data <- layers[[1]]$data
  num_edges <- length(igraph::E(g))
  expect_equal(nrow(edge_data), num_edges)
  expect_true(all(c("x", "y", "xend", "yend") %in% names(edge_data)))
})

test_that("error on non-graph input", {
  expect_error(geom_popgraph("not a graph"), "igraph or popgraph")
  expect_error(geom_popgraph(data.frame(x = 1)), "igraph or popgraph")
})

test_that("custom node.size, edge.color, edge.width are applied", {
  g <- make_test_graph()
  layers <- geom_popgraph(g, aes(x = x, y = y),
                          node.size = 5, edge.color = "red", edge.width = 1.5)
  # Edge layer fixed params
  edge_params <- layers[[1]]$aes_params
  expect_equal(edge_params$colour, "red")
  expect_equal(edge_params$linewidth, 1.5)
  # Node layer fixed params
  node_params <- layers[[2]]$aes_params
  expect_equal(node_params$size, 5)
})

test_that("works with ggplot addition", {
  g <- make_test_graph()
  p <- ggplot2::ggplot() + geom_popgraph(g, aes(x = x, y = y))
  expect_is(p, "ggplot")
  # Should have 2 layers (edge + node)
  expect_equal(length(p$layers), 2)
})
