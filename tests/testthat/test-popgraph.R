context("popgraph.R")

# Structural regression coverage for popgraph() itself. Until now this file
# only carried a commented-out theoretical note — no real assertions ever
# ran against the core Dyer & Nason (2004) algorithm. Uses the arapat and
# cornus datasets already bundled with the package (different marker types,
# population counts, and allele-label shapes), mirroring the equivalent
# structural + numerical-identity test suite added to the Swift port
# (DyerLabFoundation's PopulationGraphTests.swift), which validates its
# output against this very function's output on the same two datasets.

test_that("popgraph on arapat produces a well-formed popgraph", {
  data(arapat)
  mv <- to_mv(arapat[, -c(1:6)])
  g <- popgraph(x = mv, groups = arapat$Population)

  expect_s3_class(g, "igraph")
  expect_s3_class(g, "popgraph")
  expect_true(igraph::is_weighted(g))
  expect_false(igraph::is_directed(g))
  expect_equal(length(igraph::V(g)), length(unique(arapat$Population)))
  expect_equal(sum(igraph::which_loop(g)), 0)
  expect_true(all(igraph::E(g)$weight > 0))
  expect_true(all(igraph::V(g)$size > 0))
})

test_that("popgraph on cornus produces a well-formed popgraph", {
  data(cornus)
  mv <- to_mv(cornus[, -c(1:4)])
  g <- popgraph(x = mv, groups = cornus$Population)

  expect_s3_class(g, "igraph")
  expect_s3_class(g, "popgraph")
  expect_true(igraph::is_weighted(g))
  expect_false(igraph::is_directed(g))
  expect_equal(length(igraph::V(g)), length(unique(cornus$Population)))
  expect_equal(sum(igraph::which_loop(g)), 0)
  expect_true(all(igraph::E(g)$weight > 0))
  expect_true(all(igraph::V(g)$size > 0))
})

test_that("popgraph edge count matches a pinned regression baseline (arapat)", {
  # Pins the current, literature-validated behavior so a future change to
  # popgraph()'s internals (CDA rotation, Gower transform, chi-square
  # threshold, etc.) that alters retained edges is caught rather than
  # silently shipped.
  data(arapat)
  mv <- to_mv(arapat[, -c(1:6)])
  g <- popgraph(x = mv, groups = arapat$Population)

  expect_equal(igraph::ecount(g), 71)
})

test_that("popgraph edge count matches a pinned regression baseline (cornus)", {
  data(cornus)
  mv <- to_mv(cornus[, -c(1:4)])
  g <- popgraph(x = mv, groups = cornus$Population)

  expect_equal(igraph::ecount(g), 10)
})

test_that("the minimum-variance population gets the minimum node size", {
  # Node size is scale(allSD, center=min(allSD), scale=TRUE)*5+5, so the
  # population with the smallest within-group variance always maps to
  # exactly 5 — a cheap, exact check on that formula without hand-deriving
  # every node's size.
  data(arapat)
  mv <- to_mv(arapat[, -c(1:6)])
  g <- popgraph(x = mv, groups = arapat$Population)

  expect_equal(min(igraph::V(g)$size), 5)
})
