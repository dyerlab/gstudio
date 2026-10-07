#' Plot a gravity congruence
#'
#' @description
#' Maps where two graphs agree about genetic gravity.  The surface is the
#' \emph{shared} source-sink score: at each population the mean of the two
#' standardized scores (\eqn{S / sd(S)}) when both graphs agree that it is a
#' source (red) or a sink (blue), and 0 (white) when they disagree.  The shared
#' edges are drawn; an arrow on each edge whose gene-flow direction agrees
#' points from source to sink, with length proportional to the geometric mean
#' of the two \eqn{|\Delta|}, and edges whose directions disagree are overlaid
#' with a dashed line.  Nodes are coloured by their \code{concordance} class.
#'
#' Uses the same plotting backend as \code{\link{plot.popgraph}}, so
#' \code{layout}, \code{node_size}, \code{node_labels}, \code{node_fill},
#' \code{edges} and \code{edge_width} work the same way.
#'
#' @inheritParams plot.gravity_field
#' @param x A \code{\link{gravity_congruence}}.
#' @param layout How to place the nodes; see \code{\link{plot.popgraph}}.  Named
#'   and function layouts are computed on the union of the two graphs (mean
#'   weight on shared edges), so every population is placed by its connections
#'   in either graph.
#' @param node_size \code{"size"} uses a \code{size} vertex attribute if the
#'   congruence carries one (it usually does not, as each graph has its own
#'   \code{size}), and degree on the congruence topology otherwise.
#' @param node_fill \code{NULL} (default) colours nodes by \code{concordance}:
#'   shared source and shared sink in the \code{palette} colours, discordant in
#'   grey.  Otherwise a colour, or the name
#'   of any column of \code{x$nodes} (e.g. \code{"S.1"} or \code{"Region"}).
#' @param edge_width \code{"constant"} (default) or \code{"weight"}, which uses
#'   the mean of the two graphs' weights.
#' @param arrows Draw arrows on concordant edges and dashes on discordant ones
#'   (default \code{TRUE}).
#' @return A \code{ggplot} object.
#' @seealso \code{\link{gravity_congruence}}, \code{\link{plot.gravity_field}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' # See ?gravity_congruence
#' @export
plot.gravity_congruence <- function(x, y, ...,
                                    layout = NULL,
                                    node_size = c("size", "degree", "constant"),
                                    node_labels = c("name", "degree", "size", "none"),
                                    node_fill = NULL,
                                    edges = TRUE,
                                    edge_width = c("constant", "weight"),
                                    base = NULL,
                                    surface = c("interpolate", "voronoi", "none"),
                                    alpha = 1,
                                    arrows = TRUE,
                                    palette = c("#2166ac", "#b2182b"),
                                    mask_dist = NULL,
                                    arrow_scale = 1,
                                    grid_n = 140) {
  chkDots(...)
  .check_base(base)
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha < 0 || alpha > 1)
    stop("'alpha' must be a single number between 0 and 1.")
  if (!is.numeric(node_size)) node_size <- match.arg(node_size)
  node_labels <- match.arg(node_labels)
  edge_width  <- match.arg(edge_width)
  surface     <- match.arg(surface)

  cong <- x$graphs$congruence
  nodes <- x$nodes$Stratum
  if (is.null(node_fill)) node_fill <- "concordance"
  fill_attr <- length(node_fill) == 1 && node_fill %in% igraph::vertex_attr_names(cong)
  if (!fill_attr && (length(node_fill) != 1 ||
                     inherits(try(grDevices::col2rgb(node_fill), silent = TRUE), "try-error")))
    stop("'node_fill' must be a single colour or the name of a vertex attribute.")

  # Layouts on the union of both graphs, so populations with few shared
  # edges still sit near their neighbours.
  W <- function(g) {
    m <- igraph::as_adjacency_matrix(g, attr = "weight", sparse = FALSE)[nodes, nodes]
    m[m == 0] <- NA
    m
  }
  w1 <- W(x$graphs$graph1); w2 <- W(x$graphs$graph2)
  w <- ifelse(is.na(w1), w2, ifelse(is.na(w2), w1, (w1 + w2) / 2))
  w[is.na(w)] <- 0
  union_graph <- igraph::graph_from_adjacency_matrix(w, mode = "undirected", weighted = TRUE)
  xy <- .graph_layout(cong, layout, layout_graph = union_graph, nodes = nodes)

  nd <- data.frame(node = nodes, x = xy[nodes, 1], y = xy[nodes, 2],
                   degree = igraph::degree(cong)[nodes], panel = "",
                   size = if ("size" %in% igraph::vertex_attr_names(cong))
                     igraph::V(cong)$size else NA_real_,
                   stringsAsFactors = FALSE)
  if (fill_attr) nd <- .graph_node_fill(nd, cong, node_fill)
  node_colours <- NULL
  if (identical(node_fill, "concordance")) {
    nd$fill <- factor(nd$fill, c("source", "sink", "discordant"))
    node_colours <- c(source = palette[2], sink = palette[1], discordant = "grey70")
  }
  # Shared score: mean standardized S where the graphs agree, 0 where not.
  z <- function(s) { v <- stats::sd(s); if (is.finite(v) && v > 0) s / v else s }
  z1 <- z(x$nodes$S.1); z2 <- z(x$nodes$S.2)
  nd$shared <- ifelse(x$nodes$concordance == "discordant", 0, (z1 + z2) / 2)

  e <- x$edges
  seg <- .edge_segments(cbind(e$from, e$to), xy)
  seg$weight <- (e$weight.1 + e$weight.2) / 2
  seg$panel <- ""

  underlay <- .gravity_surface(nd, "shared", "", surface, mask_dist, grid_n, alpha)
  overlay <- list()
  if (arrows && nrow(e)) {
    edge_len <- .median_edge_length(seg)
    mag <- sqrt(abs(e$delta.1 * e$delta.2))
    ok <- !is.na(e$concordant) & e$concordant
    a <- e[ok, ]
    a$source <- ifelse(a$delta.1 >= 0, a$from, a$to)
    a$sink   <- ifelse(a$delta.1 >= 0, a$to, a$from)
    overlay <- .gravity_arrows(.edge_midpoints(a, xy), mag[ok], max(mag[ok], 0), edge_len, arrow_scale)
    dis <- seg[!is.na(e$concordant) & !e$concordant, ]
    if (nrow(dis))
      overlay <- c(overlay, list(ggplot2::geom_segment(
        data = dis, ggplot2::aes(.data$x, .data$y, xend = .data$xend, yend = .data$yend),
        colour = "black", linewidth = 0.6, linetype = "22", inherit.aes = FALSE)))
  }

  lim <- max(abs(nd$shared), na.rm = TRUE); if (!is.finite(lim) || lim == 0) lim <- 1
  p <- .graph_canvas(nd, seg, node_size = node_size, node_labels = node_labels,
                     node_fill = node_fill, edges = edges, edge_width = edge_width,
                     geographic = identical(attr(xy, "source"), "geographic"),
                     underlay = underlay, overlay = overlay, node_colours = node_colours,
                     base = base)
  t <- x$tests
  p + ggplot2::scale_fill_gradient2(low = palette[1], mid = "#f7f7f7", high = palette[2], midpoint = 0,
                                    limits = c(-lim, lim),
                                    name = "Shared source–sink score\n(red = both source,\nblue = both sink)",
                                    na.value = NA) +
    ggplot2::labs(subtitle = sprintf("S: rho = %.2f (p = %s);  edge direction: %.2f concordant (p = %s)",
                                     t$statistic[1], format.pval(t$p.value[1], digits = 2),
                                     t$statistic[2], format.pval(t$p.value[2], digits = 2)))
}
