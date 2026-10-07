# Tests for gravity_congruence() and its methods.

chain_pair <- function(K = 10, seed = 2) {
  set.seed(seed)
  nm <- paste0("P", seq_len(K))
  chain <- function(extra) {
    A <- matrix(0, K, K, dimnames = list(nm, nm))
    for (i in seq_len(K - 1)) A[i, i + 1] <- A[i + 1, i] <- stats::runif(1, 0.5, 2)
    for (e in extra) A[e[1], e[2]] <- A[e[2], e[1]] <- stats::runif(1, 2, 3)
    igraph::graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE)
  }
  list(g1 = chain(list(c(2, 5), c(6, 9))), g2 = chain(list(c(2, 5), c(3, 8))))
}
xy10 <- cbind(x = 1:10, y = 2 * sin(1:10 * pi / 5))

test_that("gravity_congruence compares S and edge direction on the shared topology", {
  gp <- chain_pair()
  set.seed(1)
  gc <- gravity_congruence(gp$g1, gp$g2, nperm = 99)
  expect_s3_class(gc, "gravity_congruence")

  s1 <- source_sink_scores(gp$g1); s2 <- source_sink_scores(gp$g2)
  expect_equal(gc$nodes$S.1, s1$S[match(gc$nodes$Stratum, s1$Stratum)])
  expect_equal(gc$nodes$S.2, s2$S[match(gc$nodes$Stratum, s2$Stratum)])
  expect_equal(gc$tests$test, c("S", "edge direction"))
  expect_equal(gc$tests$statistic[1], stats::cor(gc$nodes$S.1, gc$nodes$S.2, method = "spearman"))
  expect_true(all(c("statistic", "parameter", "p.value", "method", "alternative") %in% names(gc$tests)))
  expect_true(all(gc$tests$p.value > 0 & gc$tests$p.value <= 1))

  # shared edges match congruence_topology(); deltas match each graph's own
  ct <- congruence_topology(as.popgraph(igraph::as_adjacency_matrix(gp$g1, sparse = FALSE)),
                            as.popgraph(igraph::as_adjacency_matrix(gp$g2, sparse = FALSE)))
  expect_equal(nrow(gc$edges), igraph::ecount(ct))
  ge1 <- gravity_edges(gp$g1)
  key <- function(a, b) paste(pmin(a, b), pmax(a, b))
  i <- match(key(gc$edges$from, gc$edges$to), key(ge1$from, ge1$to))
  sgn <- ifelse(ge1$from[i] == gc$edges$from, 1, -1)
  expect_equal(gc$edges$delta.1, sgn * ge1$Delta[i])
  expect_equal(gc$edges$concordant, sign(gc$edges$delta.1) == sign(gc$edges$delta.2))
  expect_equal(gc$tests$statistic[2], mean(gc$edges$concordant))
  expect_equal(gc$tests$parameter, c(10, nrow(gc$edges)))

  # concordance classes
  cl <- with(gc$nodes, ifelse(S.1 == 0 | S.2 == 0, "neutral",
                              ifelse(S.1 > 0 & S.2 > 0, "source", ifelse(S.1 < 0 & S.2 < 0, "sink", "discordant"))))
  expect_equal(gc$nodes$concordance, cl)

  # a graph compared with itself is perfectly congruent
  set.seed(1)
  self <- gravity_congruence(gp$g1, gp$g1, nperm = 99)
  expect_equal(self$tests$statistic, c(1, 1))
  expect_true(all(self$tests$p.value < 0.05))
  expect_true(all(self$nodes$concordance != "discordant"))
})

test_that("gravity_congruence classes populations with S = 0 in either graph as neutral", {
  gp <- chain_pair()
  # P10 isolated in g2 (S = 0 there); g2 stays connected otherwise
  g2 <- igraph::delete_edges(gp$g2, igraph::incident(gp$g2, "P10"))
  set.seed(1)
  gc <- gravity_congruence(gp$g1, g2, nperm = 19)
  n <- gc$nodes[gc$nodes$Stratum == "P10", ]
  expect_equal(n$S.2, 0)
  expect_true(n$S.1 != 0)
  expect_equal(n$concordance, "neutral")
  expect_equal(sum(gc$nodes$concordance == "neutral"), 1)
  expect_equal(gc$tests$parameter[1], 10)          # still in the correlation
  expect_output(print(gc), "neutral: +P10")
  b <- ggplot2::ggplot_build(plot(gc, layout = xy10))
  expect_true("neutral" %in% levels(b$plot$layers[[length(b$plot$layers)]]$data$fill) ||
              any(vapply(b$data, function(d) "white" %in% d$colour, logical(1))))
  # no neutral nodes, no neutral line
  expect_false(grepl("neutral", paste(capture.output(print(gravity_congruence(gp$g1, gp$g2, nperm = 9))),
                                      collapse = "\n")))
})

test_that("gravity_congruence requires the same populations", {
  gp <- chain_pair()
  g3 <- igraph::delete_vertices(gp$g2, "P10")
  expect_error(gravity_congruence(gp$g1, g3), "same populations.*P10")
  g4 <- gp$g2; igraph::V(g4)$name[1] <- "Other"
  expect_error(gravity_congruence(gp$g1, g4), "Other")
  # node order does not matter
  g5 <- igraph::permute(gp$g2, c(10:1))
  set.seed(1); a <- gravity_congruence(gp$g1, gp$g2, nperm = 19)
  set.seed(1); b <- gravity_congruence(gp$g1, g5, nperm = 19)
  expect_equal(a$nodes, b$nodes)
  expect_equal(a$tests$statistic, b$tests$statistic)
})

test_that("gravity_congruence accepts single-panel gravity fields and carries decorations", {
  gp <- chain_pair()
  igraph::V(gp$g1)$Latitude <- igraph::V(gp$g2)$Latitude <- seq(20, 29)
  igraph::V(gp$g1)$size <- 1:10; igraph::V(gp$g2)$size <- 10:1
  f1 <- gravity_field(gp$g1, gamma = 1); f2 <- gravity_field(gp$g2, gamma = 1)
  gc <- gravity_congruence(f1, f2, nperm = 19)
  expect_equal(gc$gamma, c(1, 1))
  expect_equal(gc$nodes$S.1, f1$nodes$S)
  expect_equal(gc$nodes$Latitude, seq(20, 29))
  expect_equal(gc$nodes$size.1, 1:10)
  expect_false("S" %in% names(gc$nodes))
  cg <- gc$graphs$congruence
  expect_s3_class(cg, "popgraph")
  expect_equal(igraph::V(cg)$concordance, gc$nodes$concordance)
  expect_equal(igraph::E(cg)$delta.2, gc$edges$delta.2)
  expect_equal(igraph::V(gc$graphs$graph1)$S, gc$nodes$S.1)
  expect_error(gravity_congruence(gravity_field(gp$g1, gamma = c(1, 0.5)), gp$g2), "multi-panel")
  expect_error(gravity_congruence(gp$g1, 1), "'y'")
})

test_that("gravity_congruence prints, converts and plots", {
  gp <- chain_pair()
  gc <- gravity_congruence(gp$g1, gp$g2, nperm = 19)
  expect_output(print(gc), "edge direction")
  expect_equal(as.data.frame(gc, what = "tests"), gc$tests)
  expect_equal(as.data.frame(gc), gc$nodes)
  p <- plot(gc, layout = xy10)
  expect_s3_class(p, "ggplot")
  expect_equal(p$scales$get_scales("colour")$name, "concordance")
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  for (args in list(list(layout = "kk", surface = "voronoi"), list(node_fill = "S.1", edge_width = "weight"),
                    list(arrows = FALSE, surface = "none", node_fill = "grey70")))
    expect_s3_class(ggplot2::ggplot_build(do.call(plot, c(list(gc), args))), "ggplot_built")
  expect_warning(plot(gc, layout = xy10, gamma = 1), "gamma")
  shared <- c("layout", "node_size", "node_labels", "node_fill", "edges", "edge_width", "base")
  for (a in shared)
    expect_identical(formals(plot.gravity_congruence)[[a]], formals(plot.popgraph)[[a]])
})

test_that("plot.gravity_congruence takes a surface alpha", {
  gp <- chain_pair()
  gc <- gravity_congruence(gp$g1, gp$g2, nperm = 19)
  b <- ggplot2::ggplot_build(plot(gc, layout = xy10, alpha = 0.3))
  expect_true(all(b$data[[1]]$alpha == 0.3))
})

test_that("plot.gravity_congruence has a legend for concordant and discordant edges", {
  gp <- chain_pair()
  gc <- gravity_congruence(gp$g1, gp$g2, nperm = 19)
  expect_true(any(gc$edges$concordant) && any(!gc$edges$concordant))
  p <- plot(gc, layout = xy10)
  sc <- p$scales$get_scales("linetype")
  expect_equal(sc$name, "Shared edges")
  b <- ggplot2::ggplot_build(p)
  lt <- unlist(lapply(b$data, function(d) d$linetype))
  expect_true(all(c("solid", "22") %in% lt))
  # no overlays, no legend entries
  b2 <- ggplot2::ggplot_build(plot(gc, layout = xy10, arrows = FALSE))
  expect_false(any(unlist(lapply(b2$data, function(d) "22" %in% d$linetype))))
})
