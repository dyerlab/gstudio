# Tests for the genetic-gravity functions: neighbourhood_weights(), gravity_edges(),
# source_sink_scores(), msr_null(), source_sink_test(), pgd(), directional_ibgd(),
# gravity_field(), plot_gravity_field(), and the gamma / flat-kernel options of
# graph_asymmetries().

make_chain <- function(K = 10, seed = 1, extra = list(c(2, 5), c(6, 9)), prefix = "P") {
  set.seed(seed)
  nm <- paste0(prefix, seq_len(K))
  A <- matrix(0, K, K, dimnames = list(nm, nm))
  for (i in seq_len(K - 1)) A[i, i + 1] <- A[i + 1, i] <- stats::runif(1, 0.5, 2)
  for (e in extra) A[e[1], e[2]] <- A[e[2], e[1]] <- stats::runif(1, 2, 3)
  igraph::graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE)
}

test_that("neighbourhood weights sum to one and gravity balances", {
  g <- make_chain()
  for (gm in c(0.5, 1, Inf)) {
    W <- neighbourhood_weights(g, gamma = gm)
    expect_equal(unname(rowSums(W, na.rm = TRUE)), rep(1, nrow(W)), tolerance = 1e-12)
    s <- source_sink_scores(g, gamma = gm)
    ed <- gravity_edges(g, gamma = gm)
    expect_equal(s$balance, s$gravity - 1, tolerance = 1e-12)
    expect_equal(sum(s$balance), 0, tolerance = 1e-10)
    expect_equal(sum(s$S), 0, tolerance = 1e-10)
  }
})

test_that("Delta matches graph_asymmetries with the opposite sign", {
  g <- make_chain()
  ga <- graph_asymmetries(g, gamma = 0.5)
  ed <- gravity_edges(g, gamma = 0.5)
  expect_equal(ed$Delta, -igraph::E(ga)$delta, tolerance = 1e-12)
  expect_equal(igraph::E(graph_asymmetries(g, scale = 0.5))$delta, igraph::E(ga)$delta)
})

test_that("S reproduces Delta exactly on a tree", {
  g <- make_chain(extra = list())
  s <- source_sink_scores(g, gamma = 0.5)
  expect_equal(attr(s, "gradient_share"), 1, tolerance = 1e-10)
  ed <- gravity_edges(g, gamma = 0.5)
  Sv <- setNames(s$S, s$node)
  expect_equal(ed$Delta, unname(Sv[ed$from] - Sv[ed$to]), tolerance = 1e-10)
})

test_that("flat kernel is pure degree", {
  g <- make_chain()
  s <- source_sink_scores(g, gamma = Inf)
  expect_equal(attr(s, "degree_share"), 1, tolerance = 1e-12)
  expect_equal(attr(s, "cor_S_S0"), 1, tolerance = 1e-10)
  ga <- graph_asymmetries(g, scale = Inf)
  k <- igraph::degree(g)
  el <- igraph::as_edgelist(ga)
  expect_equal(igraph::E(ga)$w_away, unname(1 / k[el[, 1]]))
})

test_that("rescaling every edge weight leaves the index unchanged", {
  g <- make_chain(); g2 <- g
  igraph::E(g2)$weight <- 7.3 * igraph::E(g)$weight
  expect_equal(source_sink_scores(g2)$S, source_sink_scores(g)$S, tolerance = 1e-12)
  expect_equal(gravity_edges(g2)$Delta, gravity_edges(g)$Delta, tolerance = 1e-12)
})

test_that("small bandwidths do not underflow", {
  g <- make_chain()
  W <- neighbourhood_weights(g, gamma = 0.01)
  expect_true(all(is.finite(W[!is.na(W)])))
  expect_equal(unname(rowSums(W, na.rm = TRUE)), rep(1, nrow(W)), tolerance = 1e-12)
})

test_that("components are centred separately", {
  g <- igraph::disjoint_union(make_chain(5, extra = list()), make_chain(4, seed = 2, extra = list(), prefix = "Q"))
  s <- source_sink_scores(g)
  expect_equal(as.numeric(tapply(s$S, s$component, sum)), c(0, 0), tolerance = 1e-12)
})

test_that("msr_null keeps mean, variance and spectral power", {
  W <- matrix(0, 8, 8); for (i in 1:7) W[i, i + 1] <- W[i + 1, i] <- 1
  z <- c(3, 1, 4, 1, 5, 9, 2, 6)
  set.seed(1)
  d <- msr_null(z, W, 20)
  expect_equal(colMeans(d), rep(mean(z), 20), tolerance = 1e-10)
  expect_equal(apply(d, 2, stats::sd), rep(stats::sd(z), 20), tolerance = 1e-10)
  mem <- gstudio:::.mem_basis(W)
  expect_equal(abs(stats::cor(d[, 1], mem)), abs(stats::cor(z, mem)), tolerance = 1e-10)
})

test_that("source_sink_test is reproducible and mirror-symmetric", {
  g <- make_chain(14, seed = 3, extra = list(c(2, 6), c(7, 11), c(4, 9)))
  x <- seq_len(14)
  set.seed(9); a <- source_sink_test(g, x, nperm = 99)
  set.seed(9); b <- source_sink_test(g, 15 - x, nperm = 99)
  expect_equal(b$r, -a$r)
  expect_equal(b$p, a$p)
  expect_true(a$p > 0 && a$p <= 1)
  set.seed(9); p1 <- source_sink_test(g, x, nperm = 49, null = "permute")
  expect_s3_class(p1, "source_sink_test")
  A <- matrix(0, 14, 14); for (i in 1:13) A[i, i + 1] <- A[i + 1, i] <- 1
  expect_s3_class(source_sink_test(g, x, nperm = 49, null = "adjacency", adjacency = A), "source_sink_test")
})

test_that("pgd partitions each edge and directional_ibgd returns the fit table", {
  g <- make_chain()
  dg <- pgd(g)
  expect_true(igraph::is_directed(dg))
  el <- igraph::as_edgelist(g); w <- igraph::E(g)$weight
  P <- igraph::as_adjacency_matrix(dg, attr = "weight", sparse = FALSE)
  expect_equal(P[el] + P[el[, 2:1]], w, tolerance = 1e-12)
  r <- directional_ibgd(g, x = seq_len(igraph::vcount(g)))
  expect_named(r, c("r2_cgd", "r2_pgd_blind", "r2_pgd_dir", "delta_r2", "b_fwd", "b_rev", "slope_ratio", "preferred", "gamma"))
  expect_equal(r$delta_r2, r$r2_pgd_dir - r$r2_cgd)
})

test_that("gravity_field arrows point from source to sink", {
  g <- make_chain()
  xy <- cbind(x = seq_len(10), y = sin(seq_len(10)))
  f <- gravity_field(g, coords = xy)
  e <- f$edges; n <- f$nodes
  dx <- n$x[match(e$sink, n$node)] - n$x[match(e$source, n$node)]
  expect_true(all(sign(e$ux) == sign(dx) | abs(dx) < 1e-12))
  expect_true(all((e$delta >= 0) == (e$source == e$from)))
})

test_that("plot_gravity_field builds with common scales", {
  g <- make_chain()
  xy <- cbind(x = seq_len(10), y = sin(seq_len(10)))
  p <- plot_gravity_field(list(a = g, b = g), coords = xy, gamma = c(1, 0.5))
  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(b$layout$layout$PANEL)), 2)
})

test_that("asymmetric stepping-stone migration matrix", {
  M <- migration_matrix(5, model = "stepping_stone_1d_asym", m_fwd = 0.04, m_rev = 0.01)
  expect_equal(unname(rowSums(M)), rep(1, 5))
  expect_equal(M[2, 3], 0.04); expect_equal(M[3, 2], 0.01); expect_equal(M[1, 1], 0.96); expect_equal(M[5, 5], 0.99)
})

test_that("gravity_field is an S3 object with print, plot and as.data.frame methods", {
  g <- make_chain()
  xy <- cbind(x = seq_len(10), y = sin(seq_len(10)))
  f <- gravity_field(g, coords = xy, gamma = 0.5)
  expect_s3_class(f, "gravity_field")
  expect_equal(f$gamma, 0.5)
  expect_equal(f$coords_source, "supplied")
  expect_named(f$diagnostics, c("degree_share", "cor_S_degree", "cor_S_S0", "gradient_share"))
  expect_equal(f$diagnostics[["degree_share"]], attr(source_sink_scores(g), "degree_share"))
  expect_output(print(f), "Gravity field: 10 populations")
  expect_identical(as.data.frame(f), f$nodes)
  expect_identical(as.data.frame(f, what = "edges"), f$edges)
  expect_s3_class(plot(f), "ggplot")
  # precomputed fields keep their own bandwidth; graphs and fields can be mixed in one plot
  f1 <- gravity_field(g, coords = xy, gamma = 1)
  p <- plot_gravity_field(list(local = f1, neutral = g), coords = xy, gamma = 0.5)
  labs <- levels(ggplot2::ggplot_build(p)$layout$layout$panel)
  expect_true(any(grepl(sprintf("%.2f", f1$diagnostics[["degree_share"]]), labs)))
  expect_message(gravity_field(g), "not geographic")
  expect_equal(suppressMessages(gravity_field(g))$coords_source, "layout")
})

test_that("manuscript example graphs load and reproduce their documented values", {
  data(gravity_symmetric, package = "gstudio", envir = environment())
  data(gravity_redistributed, package = "gstudio", envir = environment())
  for (g in list(gravity_symmetric, gravity_redistributed)) {
    expect_s3_class(g, "popgraph"); expect_equal(igraph::vcount(g), 25)
    expect_equal(sort(igraph::V(g)$deme), 1:25)
    expect_equal(suppressMessages(gravity_field(g))$coords_source, "vertex attributes")
  }
  set.seed(1); ts <- source_sink_test(gravity_symmetric, x = igraph::V(gravity_symmetric)$deme)
  set.seed(1); tr <- source_sink_test(gravity_redistributed, x = igraph::V(gravity_redistributed)$deme)
  expect_equal(round(ts$r, 3), -0.092); expect_gt(ts$p, 0.05)
  expect_equal(round(tr$r, 3), -0.955); expect_lt(tr$p, 0.01)
  expect_gt(directional_ibgd(gravity_redistributed, x = igraph::V(gravity_redistributed)$deme)$delta_r2, 0)
  expect_lt(directional_ibgd(gravity_symmetric, x = igraph::V(gravity_symmetric)$deme)$delta_r2, 0)
})
