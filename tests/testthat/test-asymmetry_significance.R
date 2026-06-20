# Tests for asymmetry_significance() and its base functions.
# Graph-only modes ("bandwidth", "network") run on a hand-built popgraph.
# Data-driven modes ("permutation", "jackknife") are exercised on a small
# simulated dataset and skipped if the simulation helpers are unavailable.

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

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

expected_cols <- c("from", "to", "delta", "statistic",
                   "p_value", "ci_low", "ci_high")

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("asymmetry_significance() rejects non-igraph input", {
  expect_error(asymmetry_significance(list()),  "igraph")
})

test_that("asymmetry_significance() rejects a directed graph", {
  g <- igraph::make_ring(4, directed = TRUE)
  igraph::E(g)$weight <- 1
  expect_error(asymmetry_significance(g), "undirected")
})

test_that("asymmetry_significance() requires a weight attribute", {
  g <- igraph::make_ring(4)
  igraph::V(g)$name <- LETTERS[1:4]
  expect_error(asymmetry_significance(g, mode = "bandwidth"), "weight")
})

test_that("data-driven modes require data and groups", {
  g <- make_triangle()
  expect_error(asymmetry_significance(g, mode = "permutation"), "requires")
  expect_error(asymmetry_significance(g, mode = "jackknife"),   "requires")
})

test_that("unknown mode is rejected", {
  g <- make_triangle()
  expect_error(asymmetry_significance(g, mode = "nonsense"))
})

# ---------------------------------------------------------------------------
# Graph-only modes: return shape and value ranges
# ---------------------------------------------------------------------------

test_that("bandwidth mode returns the standard columns and valid p-values", {
  g   <- make_triangle()
  res <- asymmetry_significance(g, mode = "bandwidth", nperm = 99)

  expect_s3_class(res, "data.frame")
  expect_named(res, expected_cols)
  expect_equal(nrow(res), igraph::ecount(g))
  expect_true(all(res$p_value >= 0 & res$p_value <= 1))
  expect_true(all(is.na(res$ci_low) & is.na(res$ci_high)))
  expect_identical(attr(res, "mode"), "bandwidth")
})

test_that("network mode returns a single graph-level row", {
  g   <- make_triangle()
  res <- asymmetry_significance(g, mode = "network", nperm = 99)

  expect_named(res, expected_cols)
  expect_equal(nrow(res), 1L)
  expect_true(is.na(res$from) && is.na(res$to))
  expect_true(res$p_value >= 0 && res$p_value <= 1)
})

# ---------------------------------------------------------------------------
# Data-driven modes on a small simulated graph
# ---------------------------------------------------------------------------

simulate_small_graph <- function() {
  skip_if_not(exists("make_population"), "make_population() unavailable")
  set.seed(1)
  freqs <- data.frame(
    Locus     = rep(c("L1", "L2", "L3"), each = 2),
    Allele    = rep(c("A", "B"), times = 3),
    Frequency = c(0.5, 0.5, 0.6, 0.4, 0.3, 0.7)
  )
  pops <- do.call(rbind, lapply(LETTERS[1:5], function(p) {
    d <- make_population(freqs, N = 30)
    d$Population <- p
    d
  }))
  mv     <- to_mv(pops[, setdiff(names(pops), "Population")])
  groups <- factor(pops$Population)
  list(graph = popgraph(mv, groups), data = mv, groups = groups)
}

test_that("permutation mode returns valid per-edge p-values", {
  sg <- tryCatch(simulate_small_graph(), error = function(e) skip(conditionMessage(e)))
  res <- asymmetry_significance(sg$graph, data = sg$data, groups = sg$groups,
                                mode = "permutation", nperm = 49, pendants = "keep")
  expect_named(res, expected_cols)
  expect_equal(nrow(res), igraph::ecount(sg$graph))
  expect_true(all(res$p_value >= 0 & res$p_value <= 1))
})

test_that("jackknife mode returns ordered confidence bounds", {
  sg <- tryCatch(simulate_small_graph(), error = function(e) skip(conditionMessage(e)))
  res <- asymmetry_significance(sg$graph, data = sg$data, groups = sg$groups,
                                mode = "jackknife", nperm = 49, pendants = "keep")
  expect_named(res, expected_cols)
  ok <- !is.na(res$ci_low) & !is.na(res$ci_high)
  expect_true(all(res$ci_low[ok] <= res$ci_high[ok]))
})

# ---------------------------------------------------------------------------
# Pendant (leaf) edge handling
# ---------------------------------------------------------------------------

# Triangle A-B-C with a pendant D attached only to A (degree(D) == 1), so the
# single A-D edge is the only pendant edge.
make_pendant_graph <- function() {
  A <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  A["A", "B"] <- A["B", "A"] <- 2
  A["A", "C"] <- A["C", "A"] <- 3
  A["B", "C"] <- A["C", "B"] <- 1
  A["A", "D"] <- A["D", "A"] <- 2
  g <- igraph::graph_from_adjacency_matrix(A, mode = "undirected",
                                           weighted = TRUE, diag = FALSE)
  class(g) <- c("popgraph", class(g))
  g
}

test_that("pendants = 'warn' (default) warns but keeps leaf edges", {
  g <- make_pendant_graph()
  expect_warning(
    res <- asymmetry_significance(g, mode = "bandwidth", nperm = 49),
    "pendant")
  expect_equal(nrow(res), igraph::ecount(g))
})

test_that("pendants = 'drop' removes leaf edges and records the count", {
  g   <- make_pendant_graph()
  res <- asymmetry_significance(g, mode = "bandwidth", nperm = 49,
                                pendants = "drop")
  expect_false(any(res$from == "D" | res$to == "D"))
  expect_equal(attr(res, "pendants_dropped"), 1L)
  expect_equal(nrow(res), igraph::ecount(g) - 1L)
})

test_that("pendants = 'keep' is silent and keeps every edge", {
  g <- make_pendant_graph()
  expect_warning(
    res <- asymmetry_significance(g, mode = "bandwidth", nperm = 49,
                                  pendants = "keep"),
    regexp = NA)
  expect_equal(nrow(res), igraph::ecount(g))
})

test_that("a graph with no pendants does not warn", {
  g <- make_triangle()
  expect_warning(
    asymmetry_significance(g, mode = "bandwidth", nperm = 49),
    regexp = NA)
})
