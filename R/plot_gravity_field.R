
#' Plot the gravity field of one or more Population Graphs
#'
#' @description
#' Maps genetic gravity on node coordinates: an interpolated source-sink surface
#' (red = source, blue = sink) with contour lines, the graph's edges, and an arrow at
#' each edge midpoint pointing from source to sink with length proportional to
#' \eqn{|\Delta|}.  Nodes are sized by degree.  A named list of graphs is drawn as
#' facets on \strong{common} colour and arrow scales, so panels (for example the same
#' graph at two bandwidths, or several censuses) are directly comparable.
#'
#' @param x A \code{popgraph}/\code{igraph}, a \code{\link{gravity_field}} object, or a
#'   named list of either (one facet each; graphs and fields may be mixed).  A precomputed
#'   field is drawn as it is: its own coordinates and bandwidth are used and
#'   \code{coords} / \code{gamma} do not apply to it.
#' @param coords Node coordinates (see \code{\link{gravity_field}}), used for every graph.
#' @param gamma Bandwidth multiplier, or a vector with one value per element of \code{x}
#'   (default \code{0.5}); ignored for precomputed fields.  Use \code{gamma = c(1, 0.5)} with
#'   \code{x = list(a = g, b = g)} to compare the local mean and degree-neutral fields.
#' @param surface \code{"interpolate"} (inverse-distance weighting of \eqn{S} on a grid,
#'   with contours), \code{"voronoi"} (each grid cell takes its nearest node's \eqn{S}),
#'   or \code{"none"}.
#' @param arrows Draw source-to-sink arrows (default \code{TRUE}).
#' @param edges Draw edges in grey (default \code{TRUE}).
#' @param node_labels \code{"degree"}, \code{"name"} or \code{"none"}.
#' @param palette Two colours, sink then source (default a colour-blind-safe blue/red pair).
#' @param subtitle_stats Add the degree share (\eqn{R^2} of \eqn{\Delta} on \eqn{\Delta^0})
#'   and cor(\eqn{S}, degree) to each facet label (default \code{TRUE}).
#' @param mask_dist Surface cells farther than this from every node are left blank;
#'   default 0.75 times the median nearest-neighbour distance between nodes.
#' @param arrow_scale Arrow length multiplier (default 1).  The longest arrow over all
#'   facets is \code{arrow_scale} times the median edge length.
#' @param min_delta Hide arrows whose \eqn{|\Delta|} is below this quantile of all
#'   \eqn{|\Delta|} (default 0, show all).
#' @param grid_n Surface grid resolution per axis (default 120).
#' @return A \code{ggplot} object.
#' @seealso \code{\link{gravity_field}}, \code{\link{source_sink_scores}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' library(igraph)
#' set.seed(4)
#' K <- 12
#' A <- matrix(0, K, K, dimnames = list(paste0("P", 1:K), paste0("P", 1:K)))
#' for (i in 1:(K - 1)) A[i, i + 1] <- A[i + 1, i] <- runif(1, 0.5, 2)
#' A[2, 6] <- A[6, 2] <- 2.4; A[7, 11] <- A[11, 7] <- 2.6; A[4, 9] <- A[9, 4] <- 3
#' g <- graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE)
#' xy <- cbind(x = 1:K, y = 2 * sin(1:K * pi / 6))
#' plot_gravity_field(list(`local mean` = g, `degree-neutral` = g), coords = xy, gamma = c(1, 0.5))
#' @importFrom ggplot2 .data
#' @export
plot_gravity_field <- function(x, coords = NULL, gamma = 0.5, surface = c("interpolate", "voronoi", "none"),
                               arrows = TRUE, edges = TRUE, node_labels = c("degree", "name", "none"),
                               palette = c("#2166ac", "#b2182b"), subtitle_stats = TRUE, mask_dist = NULL,
                               arrow_scale = 1, min_delta = 0, grid_n = 120) {
  surface <- match.arg(surface); node_labels <- match.arg(node_labels)
  graphs <- if (igraph::is_igraph(x) || inherits(x, "gravity_field")) list(x) else x
  ok <- is.list(graphs) && all(vapply(graphs, function(g) igraph::is_igraph(g) || inherits(g, "gravity_field"), logical(1)))
  if (!ok) stop("'x' must be a graph, a gravity_field, or a list of them")
  if (is.null(names(graphs))) names(graphs) <- if (length(graphs) == 1) "" else paste("Graph", seq_along(graphs))
  gam <- rep_len(gamma, length(graphs))
  fields <- Map(function(g, gm) if (inherits(g, "gravity_field")) g else gravity_field(g, coords, gm), graphs, gam)
  lab <- vapply(seq_along(fields), function(i) {
    f <- fields[[i]]; base <- names(graphs)[i]
    if (!subtitle_stats) return(base)
    st <- sprintf("degree share %.2f, cor(S, k) %.2f", f$diagnostics[["degree_share"]], f$diagnostics[["cor_S_degree"]])
    if (nzchar(base)) paste0(base, "\n", st) else st
  }, character(1))
  nd <- do.call(rbind, Map(function(f, l) cbind(f$nodes, panel = l), fields, lab))
  ed <- do.call(rbind, Map(function(f, l) cbind(f$edges, panel = l), fields, lab))
  nd$panel <- factor(nd$panel, lab); ed$panel <- factor(ed$panel, lab)
  ends <- function(f) { e <- f$edges; n <- f$nodes; data.frame(x = n$x[match(e$from, n$node)], y = n$y[match(e$from, n$node)],
                                                                xend = n$x[match(e$to, n$node)], yend = n$y[match(e$to, n$node)]) }
  seg <- do.call(rbind, Map(function(f, l) cbind(ends(f), panel = l), fields, lab)); seg$panel <- factor(seg$panel, lab)
  lim <- max(abs(nd$S), na.rm = TRUE); if (!is.finite(lim) || lim == 0) lim <- 1
  xy1 <- fields[[1]]$nodes[, c("x", "y")]
  dmat <- as.matrix(stats::dist(xy1)); diag(dmat) <- Inf
  nn <- stats::median(apply(dmat, 1, min))
  if (is.null(mask_dist)) mask_dist <- 0.75 * nn
  edge_len <- stats::median(sqrt((seg$xend - seg$x)^2 + (seg$yend - seg$y)^2))
  p <- ggplot2::ggplot()
  if (surface != "none") {
    rx <- range(nd$x); ry <- range(nd$y); pad <- 0.08 * max(diff(rx), diff(ry), 1e-9)
    gx <- seq(rx[1] - pad, rx[2] + pad, length.out = grid_n); gy <- seq(ry[1] - pad, ry[2] + pad, length.out = grid_n)
    gr <- expand.grid(x = gx, y = gy)
    surf <- do.call(rbind, Map(function(f, l) {
      n <- f$nodes; d <- sqrt(outer(gr$x, n$x, "-")^2 + outer(gr$y, n$y, "-")^2)
      v <- if (surface == "voronoi") n$S[apply(d, 1, which.min)] else {
        w <- 1 / pmax(d, 1e-9)^2; as.numeric((w %*% n$S) / rowSums(w)) }
      v[apply(d, 1, min) > mask_dist] <- NA
      data.frame(gr, S = v, panel = l) }, fields, lab))
    surf$panel <- factor(surf$panel, lab)
    p <- p + ggplot2::geom_raster(data = surf, ggplot2::aes(.data$x, .data$y, fill = .data$S), interpolate = TRUE, na.rm = TRUE)
    if (surface == "interpolate")
      p <- p + ggplot2::geom_contour(data = surf[!is.na(surf$S), ], ggplot2::aes(.data$x, .data$y, z = .data$S),
                                     colour = "grey35", linewidth = 0.2, bins = 8, na.rm = TRUE)
  }
  if (edges) p <- p + ggplot2::geom_segment(data = seg, ggplot2::aes(.data$x, .data$y, xend = .data$xend, yend = .data$yend),
                                            colour = "grey55", linewidth = 0.35)
  if (arrows) {
    a <- ed; dmax <- max(abs(a$delta), na.rm = TRUE)
    if (min_delta > 0) a <- a[abs(a$delta) >= stats::quantile(abs(ed$delta), min_delta), ]
    if (nrow(a) && dmax > 0) {
      L <- arrow_scale * abs(a$delta) / dmax * edge_len
      a$x0 <- a$x_mid - a$ux * L / 2; a$y0 <- a$y_mid - a$uy * L / 2; a$x1 <- a$x_mid + a$ux * L / 2; a$y1 <- a$y_mid + a$uy * L / 2
      p <- p + ggplot2::geom_segment(data = a, ggplot2::aes(.data$x0, .data$y0, xend = .data$x1, yend = .data$y1),
                                     arrow = ggplot2::arrow(length = ggplot2::unit(0.12, "cm"), type = "closed"),
                                     colour = "grey10", linewidth = 0.45)
    }
  }
  p <- p + ggplot2::geom_point(data = nd, ggplot2::aes(.data$x, .data$y, size = .data$degree, fill = .data$S), shape = 21, colour = "grey15")
  if (node_labels != "none")
    p <- p + ggplot2::geom_text(data = nd, ggplot2::aes(.data$x, .data$y, label = if (node_labels == "degree") .data$degree else .data$node),
                                size = 2.4, colour = "black")
  p + ggplot2::scale_fill_gradient2(low = palette[1], mid = "#f7f7f7", high = palette[2], midpoint = 0, limits = c(-lim, lim),
                                    name = "S (source +)", na.value = NA) +
    ggplot2::scale_size_continuous(range = c(2.5, 6), name = "Degree") +
    ggplot2::facet_wrap(~panel) + ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}
