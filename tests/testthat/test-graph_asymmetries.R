context("graph_asymmetries.R")

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

# Triangle: A-B (w=2), A-C (w=3), B-C (w=1)
# Hand-computed reference values (see derivation below):
#   b_A = 2.5, b_B = 1.5, b_C = 2.0
#   p(B|A) = 0.5986877,  p(C|A) = 0.4013123
#   p(A|B) = 0.3392436,  p(C|B) = 0.6607564
#   p(A|C) = 0.2689414,  p(B|C) = 0.7310586
make_triangle <- function() {
  A <- matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  A["A", "B"] <- A["B", "A"] <- 2
  A["A", "C"] <- A["C", "A"] <- 3
  A["B", "C"] <- A["C", "B"] <- 1
  g <- igraph::graph_from_adjacency_matrix(A, mode = "undirected",
                                           weighted = TRUE, diag = FALSE)
  class(g) <- c("popgraph", class(g))
  g
}

# Regular graph: 4-node cycle with all edge weights equal to 2.
# All degrees = 2, all bandwidths = 2; uniform kernel => delta = 0 everywhere.
make_regular_cycle <- function(w = 2) {
  g <- igraph::make_ring(4)
  igraph::V(g)$name <- LETTERS[1:4]
  igraph::E(g)$weight <- w
  class(g) <- c("popgraph", class(g))
  g
}

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("graph_asymmetries() errors without arguments", {
  expect_error(graph_asymmetries())
})

test_that("graph_asymmetries() errors on a directed graph", {
  g <- igraph::make_ring(4, directed = TRUE)
  igraph::E(g)$weight <- 1
  expect_error(graph_asymmetries(g), "undirected")
})

test_that("graph_asymmetries() errors when weight attribute is absent", {
  g <- igraph::make_ring(4)
  igraph::V(g)$name <- LETTERS[1:4]
  expect_error(graph_asymmetries(g), "weight")
})

test_that("graph_asymmetries() errors on non-igraph input", {
  expect_error(graph_asymmetries(list()), "igraph")
  expect_error(graph_asymmetries(42),    "igraph")
})

# ---------------------------------------------------------------------------
# Return type and topology preservation
# ---------------------------------------------------------------------------

test_that("graph_asymmetries() returns an igraph object", {
  g2 <- graph_asymmetries(make_triangle())
  expect_true(igraph::is_igraph(g2))
})

test_that("graph_asymmetries() preserves popgraph class", {
  g2 <- graph_asymmetries(make_triangle())
  expect_true(inherits(g2, "popgraph"))
})

test_that("graph_asymmetries() preserves vertex count and names", {
  g  <- make_triangle()
  g2 <- graph_asymmetries(g)
  expect_equal(igraph::vcount(g2), igraph::vcount(g))
  expect_equal(sort(igraph::V(g2)$name), sort(igraph::V(g)$name))
})

test_that("graph_asymmetries() preserves edge count", {
  g  <- make_triangle()
  g2 <- graph_asymmetries(g)
  expect_equal(igraph::ecount(g2), igraph::ecount(g))
})

test_that("graph_asymmetries() does not alter original edge weights", {
  g  <- make_triangle()
  g2 <- graph_asymmetries(g)
  expect_equal(igraph::E(g2)$weight, igraph::E(g)$weight)
})

test_that("graph_asymmetries() returns an undirected graph", {
  g2 <- graph_asymmetries(make_triangle())
  expect_false(igraph::is_directed(g2))
})

# ---------------------------------------------------------------------------
# New attribute presence
# ---------------------------------------------------------------------------

test_that("graph_asymmetries() adds bandwidth vertex attribute", {
  g2 <- graph_asymmetries(make_triangle())
  expect_true("bandwidth" %in% igraph::vertex_attr_names(g2))
})

test_that("graph_asymmetries() adds w_away edge attribute", {
  g2 <- graph_asymmetries(make_triangle())
  expect_true("w_away" %in% igraph::edge_attr_names(g2))
})

test_that("graph_asymmetries() adds w_to edge attribute", {
  g2 <- graph_asymmetries(make_triangle())
  expect_true("w_to" %in% igraph::edge_attr_names(g2))
})

test_that("graph_asymmetries() adds delta edge attribute", {
  g2 <- graph_asymmetries(make_triangle())
  expect_true("delta" %in% igraph::edge_attr_names(g2))
})

# ---------------------------------------------------------------------------
# Bandwidth correctness
# ---------------------------------------------------------------------------

test_that("bandwidth equals strength / degree for all nodes", {
  g  <- make_triangle()
  g2 <- graph_asymmetries(g)
  expected_b <- igraph::strength(g) / igraph::degree(g)
  # Compare in name-sorted order to be independent of vertex ordering
  obs <- igraph::V(g2)$bandwidth
  names(obs) <- igraph::V(g2)$name
  expect_equal(obs[sort(names(obs))],
               expected_b[sort(names(expected_b))],
               tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Probability correctness
# ---------------------------------------------------------------------------

test_that("w_away and w_to are in (0, 1]", {
  g2 <- graph_asymmetries(make_triangle())
  expect_true(all(igraph::E(g2)$w_away > 0 & igraph::E(g2)$w_away <= 1))
  expect_true(all(igraph::E(g2)$w_to   > 0 & igraph::E(g2)$w_to   <= 1))
})

test_that("delta equals w_away minus w_to", {
  g2 <- graph_asymmetries(make_triangle())
  expect_equal(igraph::E(g2)$delta,
               igraph::E(g2)$w_away - igraph::E(g2)$w_to,
               tolerance = 1e-10)
})

test_that("delta is in (-1, 1)", {
  g2 <- graph_asymmetries(make_triangle())
  expect_true(all(igraph::E(g2)$delta > -1 & igraph::E(g2)$delta < 1))
})

test_that("outgoing conditional probabilities sum to 1 for each node", {
  g  <- make_triangle()
  g2 <- graph_asymmetries(g)
  el <- igraph::as_edgelist(g2, names = TRUE)
  for (u in igraph::V(g2)$name) {
    # w_away on edges where u is first endpoint
    away_idx <- which(el[, 1] == u)
    # w_to on edges where u is second endpoint
    to_idx   <- which(el[, 2] == u)
    total <- sum(igraph::E(g2)$w_away[away_idx]) +
             sum(igraph::E(g2)$w_to[to_idx])
    expect_equal(total, 1, tolerance = 1e-10,
                 label = paste("row sum for node", u))
  }
})

# ---------------------------------------------------------------------------
# Hand-computed reference values (triangle A-B-C, weights 2/3/1)
# ---------------------------------------------------------------------------

test_that("bandwidth matches hand-computed values for triangle", {
  g2 <- graph_asymmetries(make_triangle())
  b  <- igraph::V(g2)$bandwidth
  names(b) <- igraph::V(g2)$name
  expect_equal(unname(b["A"]), 2.5, tolerance = 1e-10)
  expect_equal(unname(b["B"]), 1.5, tolerance = 1e-10)
  expect_equal(unname(b["C"]), 2.0, tolerance = 1e-10)
})

test_that("w_away matches hand-computed values for triangle", {
  g2 <- graph_asymmetries(make_triangle())
  expect_equal(igraph::E(g2)[igraph::E(g2, path = c("A", "B"))]$w_away,
               0.5986877, tolerance = 1e-6)
  expect_equal(igraph::E(g2)[igraph::E(g2, path = c("A", "C"))]$w_away,
               0.4013123, tolerance = 1e-6)
  expect_equal(igraph::E(g2)[igraph::E(g2, path = c("B", "C"))]$w_away,
               0.6607564, tolerance = 1e-6)
})

test_that("w_to matches hand-computed values for triangle", {
  g2 <- graph_asymmetries(make_triangle())
  expect_equal(igraph::E(g2)[igraph::E(g2, path = c("A", "B"))]$w_to,
               0.3392436, tolerance = 1e-6)
  expect_equal(igraph::E(g2)[igraph::E(g2, path = c("A", "C"))]$w_to,
               0.2689414, tolerance = 1e-6)
  expect_equal(igraph::E(g2)[igraph::E(g2, path = c("B", "C"))]$w_to,
               0.7310586, tolerance = 1e-6)
})

test_that("delta matches hand-computed values for triangle", {
  g2 <- graph_asymmetries(make_triangle())
  expect_equal(igraph::E(g2)[igraph::E(g2, path = c("A", "B"))]$delta,
               0.2594441, tolerance = 1e-6)
  expect_equal(igraph::E(g2)[igraph::E(g2, path = c("A", "C"))]$delta,
               0.1323709, tolerance = 1e-6)
  expect_equal(igraph::E(g2)[igraph::E(g2, path = c("B", "C"))]$delta,
               -0.07030221, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Symmetry property: uniform weights on a regular graph -> delta = 0
# ---------------------------------------------------------------------------

test_that("delta is zero for all edges of a regular graph with uniform weights", {
  g2 <- graph_asymmetries(make_regular_cycle(w = 2))
  expect_equal(igraph::E(g2)$delta, rep(0, igraph::ecount(g2)),
               tolerance = 1e-10)
})

test_that("delta is invariant to the scale of uniform edge weights", {
  g_w1 <- graph_asymmetries(make_regular_cycle(w = 1))
  g_w5 <- graph_asymmetries(make_regular_cycle(w = 5))
  expect_equal(igraph::E(g_w1)$delta, igraph::E(g_w5)$delta,
               tolerance = 1e-10)
})
