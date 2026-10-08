# Tests for the genetic-gravity functions: neighbourhood_weights(), gravity_edges(),
# source_sink_scores(), msr_null(), source_sink_test(), pgd(), ibgd(),
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
  Sv <- setNames(s$S, s$Stratum)
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
  expect_equal(b$statistic, -a$statistic)
  expect_equal(b$p.value, a$p.value)
  expect_true(a$p.value > 0 && a$p.value <= 1)
  expect_s3_class(a, c("source_sink_test", "htest"))
  expect_named(a$statistic, "r")
  expect_equal(unname(a$parameter), c(14, 99))
  expect_output(print(a), "Source-sink gradient test")
  expect_output(print(a), "Sources at")
  set.seed(9); p1 <- source_sink_test(g, x, nperm = 49, null = "permute")
  expect_s3_class(p1, "source_sink_test")
  expect_match(p1$method, "permutation of S")
  A <- matrix(0, 14, 14); for (i in 1:13) A[i, i + 1] <- A[i + 1, i] <- 1
  expect_s3_class(source_sink_test(g, x, nperm = 49, null = "adjacency", adjacency = A), "source_sink_test")
})

test_that("pgd partitions each edge and ibgd returns htest results for both modes", {
  g <- make_chain()
  dg <- pgd(g)
  expect_true(igraph::is_directed(dg))
  el <- igraph::as_edgelist(g); w <- igraph::E(g)$weight
  P <- igraph::as_adjacency_matrix(dg, attr = "weight", sparse = FALSE)
  expect_equal(P[el] + P[el[, 2:1]], w, tolerance = 1e-12)
  x <- seq_len(igraph::vcount(g))

  set.seed(1)
  rc <- ibgd(g, x = x)
  expect_s3_class(rc, c("ibgd", "htest"))
  expect_equal(rc$mode, "cgd")
  expect_named(rc$statistic, "r"); expect_named(rc$estimate, c("R2", "slope"))
  expect_equal(unname(rc$estimate["R2"]), unname(rc$statistic)^2)
  expect_true(rc$p.value > 0 && rc$p.value <= 1)
  expect_equal(unname(rc$parameter["nperm"]), 999)
  expect_output(print(rc), "Mantel permutation test")

  rp <- ibgd(g, x = x, mode = "pgd")
  expect_named(rp$statistic, "delta R2")
  expect_null(rp$p.value)
  expect_equal(unname(rp$statistic), unname(rp$estimate["adj R2 pGD"] - rp$estimate["R2 cGD"]))
  expect_equal(unname(rp$estimate["R2 cGD"]), unname(rc$estimate["R2"]))
  expect_equal(unname(rp$estimate["slope ratio"]),
               unname(rp$estimate["forward slope"] / rp$estimate["reverse slope"]))
  expect_output(print(rp), "No p-value")

  # a distance matrix gives the same answer as positions
  D <- abs(outer(x, x, "-")); Fw <- outer(x, x, function(a, b) b > a)
  expect_equal(ibgd(g, distance = D, forward = Fw, mode = "pgd")$statistic, rp$statistic)
  expect_error(ibgd(g, distance = D, mode = "pgd"), "forward")
  expect_error(ibgd(g), "Supply")
  expect_error(ibgd(g, x = x, mode = "bogus"))
})

test_that("gravity_field arrows point from source to sink", {
  g <- make_chain()
  xy <- cbind(x = seq_len(10), y = sin(seq_len(10)))
  f <- gravity_field(g)
  e <- f$edges
  expect_true(all((e$delta >= 0) == (e$source == e$from)))
  # drawn arrows run from the source's coordinates toward the sink's
  b <- ggplot2::ggplot_build(plot(f, layout = xy, surface = "none"))
  arr <- b$data[[2]]
  dx <- xy[match(e$sink, igraph::V(g)$name), 1] - xy[match(e$source, igraph::V(g)$name), 1]
  dx <- dx[abs(e$delta) > 1e-6]
  expect_true(all(sign(arr$xend - arr$x) == sign(dx) | abs(dx) < 1e-12))
})

test_that("multi-panel gravity fields plot with common scales", {
  g <- make_chain()
  xy <- cbind(x = seq_len(10), y = sin(seq_len(10)))
  p <- plot(gravity_field(list(a = g, b = g), gamma = c(1, 0.5)), layout = xy)
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
  f <- gravity_field(g, gamma = 0.5)
  expect_s3_class(f, "gravity_field")
  expect_named(f, c("nodes", "edges", "diagnostics", "graphs"))
  expect_equal(f$diagnostics$gamma, 0.5)
  expect_equal(f$diagnostics$panel, "")
  expect_named(f$diagnostics, c("panel", "gamma", "degree_share", "cor_S_degree", "cor_S_S0", "gradient_share"))
  expect_equal(f$diagnostics$degree_share, attr(source_sink_scores(g), "degree_share"))
  expect_output(print(f), "Gravity field: 10 populations")
  expect_identical(as.data.frame(f), f$nodes)
  expect_identical(as.data.frame(f, what = "edges"), f$edges)
  expect_identical(as.data.frame(f, what = "diagnostics"), f$diagnostics)
  expect_false(any(c("x", "y") %in% names(f$nodes)))     # coordinates are a plot-time choice
  expect_s3_class(plot(f, layout = xy), "ggplot")
  expect_s3_class(plot(f), "ggplot")

  # one graph at several gamma -> one panel per gamma
  f2 <- gravity_field(g, gamma = c(1, 0.5))
  expect_equal(f2$diagnostics$panel, c("gamma = 1", "gamma = 0.5"))
  expect_output(print(f2), "Gravity field \\[gamma = 1\\]")

  # c() combines fields; each keeps its own bandwidth and argument names become panels
  f1 <- gravity_field(g, gamma = 1)
  fc <- c(local = f1, neutral = f)
  expect_equal(fc$diagnostics$panel, c("local", "neutral"))
  expect_equal(fc$diagnostics$gamma, c(1, 0.5))
  expect_equal(unique(fc$nodes$panel), c("local", "neutral"))
  labs <- levels(ggplot2::ggplot_build(plot(fc, layout = xy))$layout$layout$panel)
  expect_true(any(grepl(sprintf("%.2f", f1$diagnostics$degree_share), labs)))
  expect_error(c(f, f), "unique")
  expect_error(c(f, 1), "gravity_field")

  # display-only arguments: mistyped or computation arguments are reported
  expect_warning(plot(f, layout = xy, gamma = 1), "gamma")
  expect_error(plot(f, layout = xy, node_fill = "notacolour"), "single colour")
})

test_that("the gravity genotypes rebuild the documented example graph and values", {
  data(gravity, package = "gstudio", envir = environment())
  expect_equal(dim(gravity), c(2500L, 22L))
  expect_equal(length(column_class(gravity, "locus")), 20L)
  g <- popgraph(to_mv(gravity), gravity$Population)
  expect_s3_class(g, "popgraph")
  expect_equal(igraph::vcount(g), 25); expect_equal(igraph::ecount(g), 81)
  deme <- as.integer(sub("Pop", "", igraph::V(g)$name))

  set.seed(1); tr <- source_sink_test(g, x = deme)
  expect_equal(round(unname(tr$statistic), 3), -0.955); expect_lt(tr$p.value, 0.01)
  di <- ibgd(g, x = deme, mode = "pgd")
  expect_equal(round(unname(di$statistic), 2), 0.16); expect_lt(di$estimate[["slope ratio"]], 1)
  set.seed(1); ic <- ibgd(g, x = deme)
  expect_equal(round(unname(ic$estimate["R2"]), 2), 0.67); expect_lt(ic$p.value, 0.01)
  expect_equal(round(attr(source_sink_scores(g), "degree_share"), 2), 0.02)

  # Test layout override and node_size
  p_kk <- plot(gravity_field(g), layout = "kk", node_size = "degree", node_labels = "degree")
  expect_s3_class(p_kk, "ggplot")

  # Near-zero pGD arcs used to collapse node pairs under "kk", blanking the
  # surface and leaving geom_contour() with zero contours.
  f_r <- gravity_field(g)
  xy <- .graph_layout(g, "kk", layout_graph = pgd(g, output = "graph"))
  dm <- as.matrix(stats::dist(xy)); diag(dm) <- Inf
  expect_gt(min(dm), 0.05 * diff(range(xy[, 1])))
  expect_no_warning(ggplot2::ggplot_build(plot(f_r, layout = "kk")))
})


test_that("source_sink_test treats near-ties as ties and trims edge_margin from each end", {
  # S values a few ulps apart that straddle a 12-digit rounding boundary
  z <- c(0.1234567890125 - 2e-16, 0.1234567890125 + 2e-16, 0.5, -0.3)
  expect_false(signif(z[1], 12) == signif(z[2], 12))
  m <- gstudio:::.merge_ties(z)
  expect_identical(m[1], m[2])
  expect_equal(m[3:4], z[3:4])
  expect_equal(gstudio:::.merge_ties(c(1, NA, 1 + 1e-15)), c(1, NA, 1))

  K <- 12
  A <- matrix(0, K, K, dimnames = list(paste0("P", 1:K), paste0("P", 1:K)))
  for (i in 1:(K - 1)) A[i, i + 1] <- A[i + 1, i] <- i / 4 + 0.5
  g <- igraph::graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE)
  set.seed(1)
  t2 <- source_sink_test(g, x = 1:K, nperm = 19, interior = TRUE)
  expect_equal(unname(t2$parameter["nodes"]), K - 4)      # keeps 3..10
  t1 <- source_sink_test(g, x = 1:K, nperm = 19, interior = TRUE, edge_margin = 1)
  expect_equal(unname(t1$parameter["nodes"]), K - 2)      # keeps 2..11
})

test_that("gravity_field stores S and gravity as vertex attributes on its graphs", {
  g <- make_chain()
  f <- gravity_field(g, gamma = c(1, 0.5))
  for (p in names(f$graphs)) {
    gp <- f$graphs[[p]]
    nd <- f$nodes[f$nodes$panel == p, ]
    expect_equal(igraph::V(gp)$S, nd$S[match(igraph::V(gp)$name, nd$Stratum)])
    expect_equal(igraph::V(gp)$gravity, nd$gravity[match(igraph::V(gp)$name, nd$Stratum)])
  }
})

test_that("gravity_field carries graph decorations into nodes, edges and graphs", {
  g <- make_chain()
  igraph::V(g)$Region <- rep(c("north", "south"), length.out = igraph::vcount(g))
  igraph::V(g)$alpha <- 0.1
  igraph::E(g)$kind <- paste0("e", seq_len(igraph::ecount(g)))
  f <- gravity_field(g)
  expect_equal(f$nodes$Region, igraph::V(g)$Region)
  expect_equal(f$edges$kind, igraph::E(g)$kind)
  expect_equal(names(f$nodes)[1:6], c("panel", "Stratum", "degree", "size", "S", "gravity"))
  gf <- f$graphs[[1]]
  expect_equal(igraph::V(gf)$Region, igraph::V(g)$Region)
  expect_equal(igraph::E(gf)$kind, igraph::E(g)$kind)
  expect_equal(igraph::E(gf)$delta, f$edges$delta)
  expect_equal(unname(igraph::ends(gf, igraph::E(gf))), unname(as.matrix(f$edges[c("from", "to")])))

  # computed columns win over clashing attributes
  igraph::V(g)$S <- 99
  expect_false(any(gravity_field(g)$nodes$S == 99))

  # panels with different decorations are filled with NA
  h <- make_chain(seed = 2)
  f2 <- c(a = f, b = gravity_field(h))
  expect_true(all(is.na(f2$nodes$Region[f2$nodes$panel == "b"])))
  expect_equal(f2$nodes$Region[f2$nodes$panel == "a"], igraph::V(g)$Region)

  # decorations do not leak into the plot (e.g. a vertex 'alpha')
  p <- plot(f, layout = "fr")
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ibgd uses great-circle distances from node coordinates only when x and distance are NULL", {
  data(arapat)
  g  <- suppressWarnings(population_graph(arapat, decorate = TRUE))
  g0 <- suppressWarnings(population_graph(arapat))
  D  <- strata_distance(strata_coordinates(arapat), mode = "Circle")

  set.seed(3); auto <- ibgd(g, nperm = 49)
  set.seed(3); man  <- ibgd(g, distance = D, nperm = 49)
  expect_equal(auto$statistic, man$statistic)
  expect_equal(auto$p.value, man$p.value)
  expect_equal(auto$data.name, "g and great-circle distance (km)")

  # explicit x or distance wins over coordinates
  xv <- igraph::V(g)$Latitude
  set.seed(3); by_x <- ibgd(g, x = xv, nperm = 49)
  set.seed(3); by_x0 <- ibgd(g0, x = xv, nperm = 49)
  expect_equal(by_x$statistic, by_x0$statistic)

  # no coordinates, no inputs: still an error
  expect_error(ibgd(g0), "Longitude")
  # pgd needs a direction
  expect_error(ibgd(g, mode = "pgd"), "forward")
  # missing coordinates
  igraph::V(g)$Latitude[1] <- NA
  expect_error(ibgd(g), "lack")
})

test_that("plot.gravity_field maps node_fill to a vertex attribute alongside the S surface", {
  g <- make_chain()
  igraph::V(g)$Region <- rep(c("north", "south"), length.out = igraph::vcount(g))
  igraph::V(g)$Elevation <- seq_len(igraph::vcount(g)) * 10
  xy <- cbind(x = seq_len(igraph::vcount(g)), y = 0)
  f <- gravity_field(g)
  scale_name <- function(p, a) { sc <- p$scales$get_scales(a); if (is.null(sc)) NULL else sc$name }

  p <- plot(f, layout = xy)                               # region attribute used by default
  expect_equal(scale_name(p, "colour"), "Region")
  expect_match(scale_name(p, "fill"), "Source")          # the surface keeps the fill scale
  b <- ggplot2::ggplot_build(p)
  dots <- b$data[[which(vapply(b$plot$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))[1]]]
  expect_equal(length(unique(dots$colour)), 2L)

  p2 <- plot(f, layout = xy, node_fill = "Elevation")     # continuous
  expect_equal(scale_name(p2, "colour"), "Elevation")
  expect_s3_class(ggplot2::ggplot_build(p2), "ggplot_built")
  expect_equal(scale_name(plot(f, layout = xy, node_fill = "S"), "colour"), "S")
  expect_null(scale_name(plot(f, layout = xy, node_fill = "grey70"), "colour"))

  # a panel whose graph lacks the attribute still plots
  fc <- c(a = f, b = gravity_field(make_chain(seed = 2)))
  expect_s3_class(ggplot2::ggplot_build(plot(fc, layout = xy, node_fill = "Elevation")), "ggplot_built")
})

test_that("plot.gravity_field alpha sets the surface opacity only", {
  g <- make_chain()
  xy <- cbind(x = seq_len(igraph::vcount(g)), y = sin(seq_len(igraph::vcount(g))))
  f <- gravity_field(g)
  surf_alpha <- function(p) {
    l <- p$layers[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomRaster"), logical(1)))]]
    l$aes_params$alpha
  }
  expect_equal(surf_alpha(plot(f, layout = xy)), 1)
  p <- plot(f, layout = xy, alpha = 0.4)
  expect_equal(surf_alpha(p), 0.4)
  b <- ggplot2::ggplot_build(p)
  expect_true(all(b$data[[1]]$alpha == 0.4))
  expect_error(plot(f, layout = xy, alpha = 2), "between 0 and 1")
  expect_error(plot(f, layout = xy, alpha = "a"), "between 0 and 1")
})

test_that("the S surface interpolates each component separately", {
  # A chain with a two-node component (S = 0 for both) set right beside it.
  g <- igraph::disjoint_union(make_chain(), make_chain(2, extra = list(), prefix = "Q"))
  xy <- rbind(cbind(x = 1:10, y = 0), cbind(x = c(4.5, 5.5), y = 0.6))
  rownames(xy) <- igraph::V(g)$name
  f <- gravity_field(g)
  q <- igraph::V(g)$name %in% c("Q1", "Q2")
  expect_true(any(abs(f$nodes$S[!q]) > 0.01))
  surf <- function(p) p$layers[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomRaster"), logical(1)))]]$data
  for (s in c("interpolate", "voronoi")) {
    d <- surf(plot(f, layout = xy, surface = s))
    d <- d[!is.na(d$value), ]
    near <- apply(sqrt(outer(d$x, xy[, 1], "-")^2 + outer(d$y, xy[, 2], "-")^2), 1, which.min)
    expect_true(any(q[near]))
    expect_equal(d$value[q[near]], rep(0, sum(q[near])), tolerance = 1e-12)
  }
})

test_that("plot.gravity_field masks the surface to the component hull or to the nodes", {
  # Twelve nodes on a circle: the centre is far from every node but inside the hull.
  g <- make_chain(12, extra = list(c(1, 12)))
  th <- seq(0, 2 * pi, length.out = 13)[-13]
  xy <- cbind(x = 10 * cos(th), y = 10 * sin(th)); rownames(xy) <- igraph::V(g)$name
  f <- gravity_field(g)
  surf <- function(p) p$layers[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomRaster"), logical(1)))]]$data
  centre <- function(d) d$value[which.min(d$x^2 + d$y^2)]
  d_hull <- surf(plot(f, layout = xy))                       # mask = "hull" is the default
  d_node <- surf(plot(f, layout = xy, mask = "nodes"))
  expect_false(is.na(centre(d_hull)))
  expect_true(is.na(centre(d_node)))
  expect_gt(sum(!is.na(d_hull$value)), sum(!is.na(d_node$value)))
  # far from every node the kernel does not underflow: values stay within the range of S
  expect_true(all(abs(d_hull$value[!is.na(d_hull$value)]) <= max(abs(f$nodes$S)) + 1e-9))
  expect_error(plot(f, layout = xy, mask = "circle"), "should be one of")
})

test_that("plot.gravity_field clips the surface to a base map's polygons", {
  g <- make_chain()
  xy <- cbind(Longitude = seq(-112, -111.1, by = 0.1), Latitude = seq(25, 25.9, by = 0.1))
  rownames(xy) <- igraph::V(g)$name
  igraph::V(g)$Longitude <- xy[, 1]; igraph::V(g)$Latitude <- xy[, 2]
  f <- gravity_field(g)
  # "land" is everything west of -111.5, drawn as one polygon
  land <- data.frame(long = c(-113, -111.5, -111.5, -113), lat = c(24, 24, 27, 27), group = 1)
  base <- ggplot2::ggplot() + ggplot2::geom_polygon(ggplot2::aes(long, lat, group = group), data = land)
  surf <- function(p) p$layers[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomRaster"), logical(1)))]]$data
  d0 <- surf(plot(f, base = base))
  d1 <- surf(plot(f, base = base, clip_to_base = TRUE))
  expect_true(any(!is.na(d0$value) & d0$x > -111.5))
  expect_false(any(!is.na(d1$value) & d1$x > -111.5))
  expect_true(any(!is.na(d1$value)))
  # the same with an sf base layer
  sq <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(cbind(c(-113, -111.5, -111.5, -113, -113),
                                                                   c(24, 24, 27, 27, 24)))), crs = 4326))
  d2 <- surf(plot(f, base = ggplot2::ggplot() + ggplot2::geom_sf(data = sq), clip_to_base = TRUE))
  expect_false(any(!is.na(d2$value) & d2$x > -111.5))
  expect_true(any(!is.na(d2$value)))
  expect_warning(plot(f, clip_to_base = TRUE), "needs a 'base'")
  expect_error(plot(f, base = base, clip_to_base = NA), "TRUE or FALSE")
})

test_that("plot.gravity_field show_graph = FALSE draws the surface alone", {
  g <- make_chain()
  xy <- cbind(x = seq_len(igraph::vcount(g)), y = sin(seq_len(igraph::vcount(g))))
  f <- gravity_field(g)
  geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(all(c("GeomSegment", "GeomPoint", "GeomText") %in% geoms(plot(f, layout = xy))))
  p <- plot(f, layout = xy, show_graph = FALSE)
  expect_setequal(geoms(p), c("GeomRaster", "GeomContour", "GeomBlank"))
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  expect_error(plot(f, layout = xy, show_graph = "no"), "TRUE or FALSE")
})

test_that("plot.gravity_field graph_alpha fades the graph, not the surface", {
  g <- make_chain()
  xy <- cbind(x = seq_len(igraph::vcount(g)), y = sin(seq_len(igraph::vcount(g))))
  f <- gravity_field(g)
  b <- ggplot2::ggplot_build(plot(f, layout = xy, graph_alpha = 0.3))
  geoms <- vapply(b$plot$layers, function(l) class(l$geom)[1], character(1))
  a <- function(k) unique(b$data[[k]]$alpha)
  expect_equal(a(which(geoms == "GeomRaster")), 1)
  for (k in which(geoms == "GeomPoint")) expect_equal(a(k), 0.3)
  expect_equal(a(which(geoms == "GeomText")), 0.3)
  seg <- which(geoms == "GeomSegment")
  expect_setequal(unlist(lapply(seg, a)), c(0.7 * 0.3, 0.3))   # edges, arrows
  # default unchanged; 0 hides the graph
  b1 <- ggplot2::ggplot_build(plot(f, layout = xy))
  expect_equal(unique(b1$data[[which(vapply(b1$plot$layers, function(l) class(l$geom)[1], "") == "GeomText")]]$alpha), 1)
  expect_setequal(vapply(plot(f, layout = xy, graph_alpha = 0)$layers, function(l) class(l$geom)[1], ""),
                  c("GeomRaster", "GeomContour", "GeomBlank"))
  expect_error(plot(f, layout = xy, graph_alpha = 2), "between 0 and 1")
})

test_that("plot.gravity_field graph_alpha sets the graph's parts separately", {
  g <- make_chain()
  xy <- cbind(x = seq_len(igraph::vcount(g)), y = sin(seq_len(igraph::vcount(g))))
  f <- gravity_field(g)
  geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1], character(1))
  p <- plot(f, layout = xy, graph_alpha = c(nodes = 0.4, labels = 0.9, edges = 0.5))
  b <- ggplot2::ggplot_build(p)
  a <- function(k) unique(b$data[[k]]$alpha)
  for (k in which(geoms(p) == "GeomPoint")) expect_equal(a(k), 0.4)
  expect_equal(a(which(geoms(p) == "GeomText")), 0.9)
  expect_setequal(unlist(lapply(which(geoms(p) == "GeomSegment"), a)), c(0.7 * 0.5, 1))  # arrows default 1
  # a part at 0 is left out; the rest stay
  g0 <- geoms(plot(f, layout = xy, graph_alpha = c(arrows = 0, nodes = 0)))
  expect_equal(sum(g0 == "GeomSegment"), 1)
  expect_false("GeomPoint" %in% g0)
  expect_true("GeomText" %in% g0)
  expect_s3_class(ggplot2::ggplot_build(plot(f, layout = xy, graph_alpha = c(nodes = 0))), "ggplot_built")
  expect_error(plot(f, layout = xy, graph_alpha = c(node = 0.5)), "not node")
  expect_error(plot(f, layout = xy, graph_alpha = c(0.2, 0.3)), "named by part")
})

test_that("plot_nodes() returns the plotted node positions, shown or not", {
  g <- make_chain()
  xy <- cbind(x = seq_len(igraph::vcount(g)), y = sin(seq_len(igraph::vcount(g))))
  f <- gravity_field(g)
  for (p in list(plot(f, layout = xy), plot(f, layout = xy, show_graph = FALSE),
                 plot(f, layout = xy) + ggplot2::labs(title = "x"))) {
    nd <- plot_nodes(p)
    expect_equal(names(nd)[1:4], c("panel", "node", "x", "y"))
    expect_equal(nd$x, unname(xy[, 1])); expect_equal(nd$y, unname(xy[, 2]))
    expect_true("S" %in% names(nd))
  }
  expect_equal(nrow(plot_nodes(plot(as.popgraph(g)))), igraph::vcount(g))
  expect_error(plot_nodes(ggplot2::ggplot()), "no node layer")
  expect_error(plot_nodes(1), "must be a ggplot")
})
