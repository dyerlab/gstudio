test_that("popgraph() records its design", {
  data(arapat)
  g <- population_graph(arapat)
  a <- attr(g, "design")
  expect_equal(a$N, nrow(arapat))
  expect_equal(a$K, nlevels(factor(arapat$Population)))
  expect_equal(a$df_within, a$N - a$K)
  expect_true(a$rank_between <= a$K - 1)
})

test_that("popgraph_diagnostics() summarizes design and flags thin sampling", {
  data(arapat)
  dg <- popgraph_diagnostics(arapat)
  expect_s3_class(dg, "popgraph_diagnostics")
  expect_equal(nrow(dg$strata), nlevels(factor(arapat$Population)))
  expect_true(all(c("Stratum", "n", "Hs", "nHs") %in% names(dg$strata)))
  expect_true(dg$design[["gst"]] > 0 && dg$design[["gst"]] < 1)
  ck <- as.data.frame(dg)
  expect_s3_class(ck, "data.frame")
  expect_named(ck, c("section", "check", "value", "threshold", "status", "note"))
  expect_true(all(ck$status %in% c("ok", "warn", NA)))
  expect_true(all(is.na(ck$status) == is.na(ck$threshold)))
  expect_equal(ck$status[ck$check == "expected hets (n*Hs)"], "warn")
  expect_output(print(dg), "need attention")
  expect_output(print(dg), "sampling diagnostics")
  g <- dplyr::group_by(arapat, Population)
  expect_equal(popgraph_diagnostics(g)$design, dg$design)
})

test_that("popgraph() drops variables constant within strata and records alpha/tol", {
  set.seed(4)
  grp <- factor(rep(LETTERS[1:5], each = 8))
  # within-stratum noise centred in every stratum is uncorrelated with the stratum
  # means, so the stratum-mean column survives the rotation as a variable that is
  # constant within strata
  e <- matrix(rnorm(40 * 3), 40); e <- e - apply(e, 2, function(z) ave(z, grp))
  x <- cbind(10 * (as.numeric(grp) - 3), e)
  g <- expect_warning(popgraph(x, grp, alpha = 0.01), "constancy within groups")
  a <- attr(g, "design")
  expect_equal(a$constant_within, 1)
  expect_equal(a$dims, 3)
  expect_equal(a$alpha, 0.01)
  expect_equal(a$tol, 1e-4)
})

test_that("popgraph_diagnostics() checks graph nodes and flags by component size", {
  data(arapat)
  g <- population_graph(arapat)
  sub <- droplevels(arapat[arapat$Population %in% names(igraph::V(g))[1:5], ])
  expect_error(popgraph_diagnostics(sub, graph = g), "nodes of 'graph'")
  dg <- popgraph_diagnostics(arapat, graph = g)
  kc <- max(igraph::components(g)$csize)
  ck <- dg$checks
  expect_equal(ck$value[ck$check == "strata in largest component"],
               sprintf("%d of %d", kc, igraph::vcount(g)))
  expect_equal(ck$status[ck$check == "strata in largest component"], if (kc < 15) "warn" else "ok")
  ck0 <- popgraph_diagnostics(arapat, graph = g, min_nHs = 0)$checks
  expect_equal(ck0$status[ck0$check == "expected hets (n*Hs)"], "ok")
})
