# Tests for asymmetry_ci(), the bootstrap confidence interval companion to
# asymmetry_significance().  Uses simulate_small_graph() from helper-asymmetry.R.

ci_cols <- c("from", "to", "delta", "boot_mean",
             "ci_low", "ci_high", "edge_support")

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("asymmetry_ci() validates the graph", {
  expect_error(asymmetry_ci(list(), data = matrix(0), groups = 1), "igraph")

  g <- igraph::make_ring(4, directed = TRUE)
  igraph::E(g)$weight <- 1
  expect_error(asymmetry_ci(g, data = matrix(0), groups = 1), "undirected")
})

test_that("asymmetry_ci() requires data and groups", {
  g <- igraph::make_ring(4)
  igraph::V(g)$name <- LETTERS[1:4]
  igraph::E(g)$weight <- 1
  expect_error(asymmetry_ci(g, data = NULL, groups = NULL), "required")
  expect_error(asymmetry_ci(g, data = data.frame(x = 1), groups = 1), "matrix")
  expect_error(asymmetry_ci(g, data = matrix(0, 3, 2), groups = 1:2), "one entry")
})

test_that("asymmetry_ci() rejects an invalid confidence level", {
  sg <- tryCatch(simulate_small_graph(), error = function(e) skip(conditionMessage(e)))
  expect_error(asymmetry_ci(sg$graph, sg$data, sg$groups, nboot = 5, conf = 1),
               "conf")
  expect_error(asymmetry_ci(sg$graph, sg$data, sg$groups, nboot = 5, conf = 0),
               "conf")
})

# ---------------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------------

test_that("asymmetry_ci() returns ordered bounds and edge support", {
  sg  <- tryCatch(simulate_small_graph(), error = function(e) skip(conditionMessage(e)))
  res <- asymmetry_ci(sg$graph, data = sg$data, groups = sg$groups,
                      nboot = 49, pendants = "keep")

  expect_s3_class(res, "data.frame")
  expect_named(res, ci_cols)
  expect_equal(nrow(res), igraph::ecount(sg$graph))

  ok <- !is.na(res$ci_low) & !is.na(res$ci_high)
  expect_true(all(res$ci_low[ok] <= res$ci_high[ok]))
  expect_true(all(res$edge_support >= 0 & res$edge_support <= 1))
  # An edge never present in any resample has no interval.
  expect_true(all(is.na(res$ci_low[res$edge_support == 0])))

  expect_equal(attr(res, "conf"), 0.95)
  expect_equal(attr(res, "nboot"), 49)
})

test_that("a wider confidence level gives intervals at least as wide", {
  sg <- tryCatch(simulate_small_graph(), error = function(e) skip(conditionMessage(e)))
  set.seed(42)
  r90 <- asymmetry_ci(sg$graph, sg$data, sg$groups, nboot = 49,
                      conf = 0.90, pendants = "keep")
  set.seed(42)
  r99 <- asymmetry_ci(sg$graph, sg$data, sg$groups, nboot = 49,
                      conf = 0.99, pendants = "keep")
  ok <- !is.na(r90$ci_low) & !is.na(r99$ci_low)
  w90 <- r90$ci_high[ok] - r90$ci_low[ok]
  w99 <- r99$ci_high[ok] - r99$ci_low[ok]
  expect_true(all(w99 >= w90 - 1e-12))
})
