# Tests for plot.popgraph() and the plotting backend it shares with
# plot.gravity_field() and animate_popgraphs().

square_graph <- function(directed = FALSE) {
  A <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  A["A", "B"] <- 1; A["B", "C"] <- 2; A["C", "D"] <- 1.5; A["A", "D"] <- 3
  if (!directed) A <- A + t(A) else A["B", "A"] <- 0.5
  g <- igraph::graph_from_adjacency_matrix(A, mode = if (directed) "directed" else "undirected",
                                           weighted = TRUE)
  class(g) <- c("popgraph", "igraph")
  g
}
xy <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1), ncol = 2, dimnames = list(LETTERS[1:4], NULL))

test_that("plot.popgraph returns a ggplot placed at the requested layout", {
  g <- square_graph()
  p <- plot(g, layout = xy)
  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  pts <- b$data[[2]]
  expect_equal(sort(pts$x), unname(sort(xy[, 1])))
  expect_equal(nrow(b$data[[1]]), igraph::ecount(g))
})

test_that("plot.popgraph options are shared with plot.gravity_field", {
  shared <- c("layout", "node_size", "node_labels", "node_fill", "edges", "edge_width", "base")
  expect_true(all(shared %in% names(formals(plot.popgraph))))
  expect_true(all(shared %in% names(formals(plot.gravity_field))))
  for (a in shared)
    expect_identical(formals(plot.popgraph)[[a]], formals(plot.gravity_field)[[a]])
})

test_that("nodes are sized by population size by default, falling back to degree", {
  size_title <- function(p) p$scales$get_scales("size")$name
  g <- square_graph()
  expect_equal(size_title(plot(g, layout = xy)), "Degree")         # no size attribute
  igraph::V(g)$size <- c(10, 20, 30, 40)
  p <- plot(g, layout = xy)
  expect_equal(size_title(p), "Size")
  pts <- ggplot2::ggplot_build(p)$data[[2]]
  expect_equal(order(pts$size), order(c(10, 20, 30, 40)[match(pts$x * 2 + pts$y, xy[, 1] * 2 + xy[, 2])]))
  expect_equal(size_title(plot(g, layout = xy, node_size = "degree")), "Degree")

  gs <- square_graph()
  expect_equal(size_title(plot(gravity_field(gs), layout = xy)), "Degree")
  igraph::V(gs)$size <- seq_len(igraph::vcount(gs))
  expect_equal(size_title(plot(gravity_field(gs), layout = xy)), "Size")
})

test_that("nodes are filled by a region attribute by default", {
  fill_name <- function(p) { sc <- p$scales$get_scales("colour"); if (is.null(sc)) NULL else sc$name }
  g <- square_graph()
  expect_null(fill_name(plot(g, layout = xy)))                     # no region: plain white
  expect_equal(.resolve_node_fill(g, NULL), "white")
  igraph::V(g)$region <- c("N", "N", "S", "S")                       # any capitalisation
  p <- plot(g, layout = xy)
  expect_equal(fill_name(p), "region")
  expect_equal(length(unique(ggplot2::ggplot_build(p)$data[[2]]$colour)), 2L)
  expect_null(fill_name(plot(g, layout = xy, node_fill = "grey80")))   # explicit colour wins
  g2 <- square_graph()
  igraph::V(g2)$Region <- c(1, 2, 3, 4)                              # numeric region -> continuous fill
  expect_equal(fill_name(plot(g2, layout = xy)), "Region")
})

test_that("plot.popgraph styling options build", {
  g <- square_graph()
  igraph::V(g)$Region <- c("N", "N", "S", "S")
  igraph::V(g)$size <- c(1, 2, 3, 4)
  for (args in list(list(node_size = "size", node_labels = "size"),
                    list(node_size = "constant", node_labels = "none"),
                    list(node_size = 3, node_labels = "degree"),
                    list(node_fill = "Region", edge_width = "weight"),
                    list(edges = FALSE, layout = "circle")))
    expect_s3_class(ggplot2::ggplot_build(do.call(plot, c(list(g), args))), "ggplot_built")
  expect_error(plot(g, node_size = "bogus"))
  expect_warning(plot(g, layuot = "kk"), "layuot")
})

test_that("directed graphs get arrows, offset for reciprocal arcs", {
  g <- square_graph(directed = TRUE)
  b <- ggplot2::ggplot_build(plot(g, layout = xy))
  seg <- b$data[[1]]
  expect_equal(nrow(seg), igraph::ecount(g))
  expect_false(is.null(ggplot2::layer_grob(plot(g, layout = xy), 1)))
  # A->B and B->A are drawn as two distinct, parallel segments
  ab <- seg[seg$y == seg$yend & abs(seg$y) < 0.5, ]
  expect_equal(nrow(ab), 2L)
  expect_false(isTRUE(all.equal(ab$y[1], ab$y[2])))
})

test_that("layout resolution: attributes, names, functions, tables and errors", {
  g <- square_graph()
  igraph::V(g)$Longitude <- xy[, 1]; igraph::V(g)$Latitude <- xy[, 2]
  m <- .graph_layout(g)
  expect_equal(attr(m, "source"), "geographic")
  expect_equal(unname(m[, 1]), unname(xy[, 1]))

  g2 <- square_graph()
  m1 <- .graph_layout(g2); m2 <- .graph_layout(g2)
  expect_identical(m1, m2)                                  # fixed seed: reproducible
  set.seed(9); r1 <- stats::runif(1); set.seed(9)
  invisible(.graph_layout(g2, "fr")); expect_equal(stats::runif(1), r1)  # caller's RNG untouched

  expect_equal(dim(.graph_layout(g2, igraph::layout_in_circle)), c(4L, 2L))
  sc <- data.frame(Stratum = rev(LETTERS[1:4]), Longitude = rev(xy[, 1]), Latitude = rev(xy[, 2]))
  m3 <- .graph_layout(g2, sc)
  expect_equal(unname(m3[, 1]), unname(xy[, 1]))
  expect_equal(attr(m3, "source"), "geographic")
  expect_equal(unname(.graph_layout(g2, unname(xy))[, 2]), unname(xy[, 2]))   # vertex order
  expect_error(.graph_layout(g2, "bogus"), "Unknown layout")
  expect_error(.graph_layout(g2, xy[1:2, ]), "no coordinates for: C, D")
})

test_that("plot methods draw on a base ggplot, under the graph", {
  g <- square_graph()
  under <- ggplot2::ggplot(data.frame(px = c(0, 1), py = c(0, 1), z = c(1, 2)),
                           ggplot2::aes(px, py, colour = z)) +      # global mapping must not leak
    ggplot2::geom_point(colour = "red")
  p <- plot(g, layout = xy, base = under)
  b <- ggplot2::ggplot_build(p)
  expect_s3_class(b, "ggplot_built")
  expect_true(inherits(p$layers[[1]]$geom, "GeomPoint"))            # base layer first (underneath)
  expect_equal(length(p$layers), length(plot(g, layout = xy)$layers) + 1L)
  expect_true(all(vapply(p$layers[-1], function(l) isFALSE(l$inherit.aes), logical(1))))
  expect_equal(p$coordinates$ratio, 1)                              # no base coord: ours is used

  # a base coordinate system, theme and axis labels are kept
  pf <- plot(g, layout = xy, base = ggplot2::ggplot() + ggplot2::coord_cartesian(xlim = c(-5, 5)) +
               ggplot2::xlab("Longitude") + ggplot2::theme_bw())
  expect_null(pf$coordinates$ratio)
  expect_equal(pf$coordinates$limits$x, c(-5, 5))
  expect_equal(pf$labels$x, "Longitude")
  expect_equal(pf$theme, (ggplot2::ggplot() + ggplot2::theme_bw())$theme)

  # gravity fields and congruences take the same argument
  f <- gravity_field(g)
  pg <- plot(f, layout = xy, base = under)
  expect_true(inherits(pg$layers[[1]]$geom, "GeomPoint"))
  expect_s3_class(ggplot2::ggplot_build(pg), "ggplot_built")
  expect_error(plot(g, base = "map"), "ggplot")
  expect_error(plot(f, base = 1), "ggplot")
})
