# Cross-implementation parity: these assertions are mirrored, with the SAME
# input graphs and the SAME RNG seeds, by the Swift port in
# DyerLabFoundation/Tests/GraphTests/AsymmetryResamplingTests.swift. The Swift
# side uses an RMersenneTwister that reproduces R's set.seed()/sample()
# bit-for-bit, so the two produce identical numbers for identical (graph, seed).
#
#   randomize_graph()      <-> RandomizeGraph(graph:mode:seed:)
#   asymmetry_bandwidth()  <-> AsymmetryBandwidthTest(graph:permutations:seed:)
#   asymmetry_network()    <-> AsymmetryNetworkTest(graph:permutations:rewire:seed:)
#
# If you change any of these algorithms, or an expected value below, update the
# Swift fixture (Tests/GraphTests/Fixtures/generate_downstream_fixtures.R) and
# the paired Swift test in the same commit.

suppressPackageStartupMessages(library(igraph))

# A fixed 6-node weighted graph (no data(), no RNG to build it).
small_graph <- function() {
  A <- matrix(0, 6, 6, dimnames = list(LETTERS[1:6], LETTERS[1:6]))
  A["A","B"] <- A["A","D"] <- A["B","C"] <- A["B","D"] <-
    A["C","D"] <- A["C","E"] <- A["C","F"] <- A["E","F"] <- 1
  A <- A + t(A)
  g <- as.popgraph(A)
  igraph::E(g)$weight <- 1
  g
}

edgelist_chr <- function(g) {
  el <- igraph::as_edgelist(g, names = TRUE)
  paste(el[, 1], el[, 2], sep = "-")
}

# ---------------------------------------------------------------------------
# randomize_graph() — exact edge list for a fixed (graph, seed)
# ---------------------------------------------------------------------------

test_that("randomize_graph('degree') is stable for seed 11 (Swift parity)", {
  set.seed(11)
  gr <- randomize_graph(small_graph(), mode = "degree")
  expect_equal(igraph::V(gr)$name, c("A", "F", "D", "B", "C", "E"))
  expect_equal(edgelist_chr(gr),
               c("A-F", "A-D", "B-C", "D-B", "B-C", "D-C", "C-E", "F-E"))
})

test_that("randomize_graph('full') is stable for seed 13 (Swift parity)", {
  set.seed(13)
  gr <- randomize_graph(small_graph(), mode = "full")
  expect_equal(igraph::ecount(gr), 8L)
  expect_equal(igraph::vcount(gr), 6L)
  expect_equal(edgelist_chr(gr),
               c("1-3", "1-5", "1-6", "2-3", "2-5", "3-6", "4-6", "5-6"))
})

# ---------------------------------------------------------------------------
# asymmetry_bandwidth() / asymmetry_network() — exact values on the cornus
# population graph for fixed seeds. The Swift side rebuilds this graph from
# popgraph_cornus (identical to popgraph(to_mv(cornus), cornus$Population)).
# ---------------------------------------------------------------------------

cornus_graph <- function() {
  data(cornus, package = "gstudio")
  g <- popgraph(to_mv(cornus), cornus$Population)
  class(g) <- c("popgraph", "igraph")
  g
}

test_that("asymmetry_bandwidth() is stable on cornus for seed 4041 (Swift parity)", {
  g <- cornus_graph()
  set.seed(4041)
  res <- asymmetry_bandwidth(g, nperm = 200)

  expect_equal(nrow(res), igraph::ecount(g))
  expect_equal(
    res$p_value,
    c(0.582089552238806, 0.6318407960199005, 0.7661691542288557,
      0.6019900497512438, 0.7910447761194029, 0.24875621890547264,
      0.4925373134328358, 0.572139303482587, 0.208955223880597,
      0.845771144278607),
    tolerance = 1e-12)
})

test_that("asymmetry_network() is stable on cornus for seed 5051 (Swift parity)", {
  g <- cornus_graph()
  set.seed(5051)
  res <- asymmetry_network(g, nperm = 200, rewire = "degree")

  expect_equal(nrow(res), 1L)
  expect_equal(res$delta, 0.026752013114304994, tolerance = 1e-9)
  expect_equal(res$p_value, 0.03482587064676617, tolerance = 1e-12)
})
