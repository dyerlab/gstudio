# Tests for asymmetry_significance() and its base functions.
# Graph-only modes ("mechanism", "existence") run on a hand-built popgraph.
# The data-driven "location" mode is exercised on a small simulated dataset
# (see helper-asymmetry.R) and skipped if the simulation helpers are
# unavailable.  Confidence intervals are tested in test-asymmetry_ci.R.

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

expected_cols <- c("from", "to", "delta", "statistic", "p.value")

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
  expect_error(asymmetry_significance(g, mode = "mechanism"), "weight")
})

test_that("location mode requires data and groups", {
  g <- make_triangle()
  expect_error(asymmetry_significance(g, mode = "location"), "requires")
})

test_that("unknown mode is rejected, including the retired 'support' mode", {
  g <- make_triangle()
  expect_error(asymmetry_significance(g, mode = "nonsense"))
  expect_error(asymmetry_significance(g, mode = "support"))
})

test_that("data/groups without mode = 'location' warns", {
  g <- make_triangle()
  expect_warning(asymmetry_significance(g, data = matrix(0, 3, 2), groups = 1:3,
                                        nperm = 9),
                 "only used by mode = 'location'")
})

test_that("location mode ignores alpha with a warning instead of failing", {
  sg <- tryCatch(simulate_small_graph(), error = function(e) skip(conditionMessage(e)))
  set.seed(1)
  expect_warning(
    res <- asymmetry_significance(sg$graph, data = sg$data, groups = sg$groups,
                                  mode = "location", nperm = 5, alpha = 0.01,
                                  pendants = "keep"),
    "'alpha' is ignored")
  expect_true(all(res$p.value > 0 & res$p.value <= 1))
})

test_that("groups that do not match the graph nodes are rejected", {
  sg <- tryCatch(simulate_small_graph(), error = function(e) skip(conditionMessage(e)))
  bad <- factor(rep(c("X", "Y", "Z"), length.out = length(sg$groups)))
  expect_error(asymmetry_significance(sg$graph, data = sg$data, groups = bad,
                                      mode = "location", nperm = 5),
               "no individuals for these graph nodes")
  expect_error(asymmetry_ci(sg$graph, sg$data, bad, nboot = 5),
               "no individuals for these graph nodes")
})

test_that("existence mode survives rewired graphs with isolated nodes", {
  g <- make_triangle()
  set.seed(1)
  res <- asymmetry_significance(g, nperm = 20, rewire = "full")
  expect_s3_class(res, "htest")
  expect_true(res$p.value > 0 && res$p.value <= 1)
})

# ---------------------------------------------------------------------------
# Graph-only modes: return shape and value ranges
# ---------------------------------------------------------------------------

test_that("mechanism mode returns the standard columns and valid p-values", {
  g   <- make_triangle()
  res <- asymmetry_significance(g, mode = "mechanism", nperm = 99)

  expect_s3_class(res, "data.frame")
  expect_named(res, expected_cols)
  expect_equal(nrow(res), igraph::ecount(g))
  expect_true(all(res$p.value >= 0 & res$p.value <= 1))
  expect_identical(attr(res, "mode"), "mechanism")
})

test_that("existence mode returns a graph-level htest", {
  g   <- make_triangle()
  res <- asymmetry_significance(g, mode = "existence", nperm = 99)

  expect_s3_class(res, "htest")
  expect_named(res$statistic, "mean |Delta|")
  expect_identical(res$alternative, "greater")
  expect_identical(res$data.name, "g")
  expect_true(res$p.value >= 0 && res$p.value <= 1)
  expect_output(print(res), "Graph-level asymmetry test")
})

test_that("permutation p-values are strictly positive (add-one correction)", {
  # The add-one estimator (1 + #{>=}) / (1 + B) can never be exactly zero, even
  # when no permutation exceeds the observed statistic.
  g  <- make_triangle()
  rb <- asymmetry_significance(g, mode = "mechanism", nperm = 99)
  rn <- asymmetry_significance(g, mode = "existence",   nperm = 99)
  expect_true(all(rb$p.value > 0))
  expect_true(rn$p.value > 0)
  # Smallest attainable value is 1 / (1 + nperm).
  expect_true(all(rb$p.value >= 1 / (99 + 1) - 1e-12))
})

# ---------------------------------------------------------------------------
# Data-driven modes on a small simulated graph
# ---------------------------------------------------------------------------

test_that("location mode returns valid per-edge p-values", {
  sg <- tryCatch(simulate_small_graph(), error = function(e) skip(conditionMessage(e)))
  res <- asymmetry_significance(sg$graph, data = sg$data, groups = sg$groups,
                                mode = "location", nperm = 49, pendants = "keep")
  expect_named(res, expected_cols)
  expect_equal(nrow(res), igraph::ecount(sg$graph))
  expect_true(all(res$p.value >= 0 & res$p.value <= 1))
  # statistic is Delta centred on each edge's null mean, not Delta itself
  expect_true(all(is.finite(res$statistic)))
  expect_false(isTRUE(all.equal(res$statistic, res$delta)))
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
    res <- asymmetry_significance(g, mode = "mechanism", nperm = 49),
    "pendant")
  expect_equal(nrow(res), igraph::ecount(g))
})

test_that("pendants = 'drop' removes leaf edges and records the count", {
  g   <- make_pendant_graph()
  res <- asymmetry_significance(g, mode = "mechanism", nperm = 49,
                                pendants = "drop")
  expect_false(any(res$from == "D" | res$to == "D"))
  expect_equal(attr(res, "pendants_dropped"), 1L)
  expect_equal(nrow(res), igraph::ecount(g) - 1L)
})

test_that("pendants = 'keep' is silent and keeps every edge", {
  g <- make_pendant_graph()
  expect_warning(
    res <- asymmetry_significance(g, mode = "mechanism", nperm = 49,
                                  pendants = "keep"),
    regexp = NA)
  expect_equal(nrow(res), igraph::ecount(g))
})

test_that("a graph with no pendants does not warn", {
  g <- make_triangle()
  expect_warning(
    asymmetry_significance(g, mode = "mechanism", nperm = 49),
    regexp = NA)
})

test_that("return_null keeps the null distribution for every mode", {
  data(lopho)
  set.seed(3)
  ex <- asymmetry_significance(lopho, mode = "existence", nperm = 9, return_null = TRUE)
  expect_length(ex$null_distribution, unname(ex$parameter["nperm"]))
  expect_null(asymmetry_network(lopho, nperm = 3)$null_distribution)

  me <- suppressWarnings(asymmetry_significance(lopho, mode = "mechanism", nperm = 9, return_null = TRUE))
  nd <- attr(me, "null_distribution")
  expect_equal(dim(nd), c(nrow(me), 9))

  dr <- asymmetry_significance(lopho, mode = "mechanism", nperm = 9, return_null = TRUE, pendants = "drop")
  expect_equal(nrow(attr(dr, "null_distribution")), nrow(dr))

  data(arapat)
  mv <- to_mv(arapat)
  g  <- popgraph(mv, groups = arapat$Population)
  lo <- asymmetry_permutation(g, mv, arapat$Population, nperm = 3, return_null = TRUE)
  expect_equal(nrow(attr(lo, "null_distribution")), nrow(lo))
  expect_lte(ncol(attr(lo, "null_distribution")), 3)
  # the p-values can be recomputed from the returned null
  nd <- attr(lo, "null_distribution")
  ctr <- rowMeans(nd)
  expect_equal(lo$p.value, (1 + rowSums(abs(nd - ctr) >= abs(lo$delta - ctr))) / (1 + ncol(nd)))
})
