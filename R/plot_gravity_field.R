#' Plot a gravity field
#'
#' @description
#' Maps genetic gravity: an interpolated source-sink surface (red = source,
#' blue = sink) with contour lines, the graph's edges, an arrow at each edge
#' midpoint pointing from source to sink with length proportional to
#' \eqn{|\Delta|}, and the nodes.  A multi-panel field (see
#' \code{\link{gravity_field}}) is drawn as facets on \strong{common} colour and
#' arrow scales, so panels (for example the same graph at two bandwidths, or
#' several censuses) are directly comparable.
#'
#' Uses the same plotting backend as \code{\link{plot.popgraph}}, so
#' \code{layout}, \code{node_size}, \code{node_labels}, \code{node_fill},
#' \code{edges} and \code{edge_width} work the same way; the remaining arguments
#' are specific to gravity fields.
#'
#' @inheritParams plot.popgraph
#' @param x A \code{\link{gravity_field}}.
#' @param layout How to place the nodes; see \code{\link{plot.popgraph}}.  Named
#'   and function layouts are computed on the bi-directional \code{\link{pgd}}
#'   graph, so directional gene flow shapes the picture.  When every panel has
#'   the same populations, one layout (from the first panel) is used for all of
#'   them.
#' @param node_fill As for \code{\link{plot.popgraph}}: \code{NULL} (default)
#'   uses a \code{region} vertex attribute when present and white otherwise,
#'   or give a colour, or the name of any vertex attribute of the field's graphs
#'   (including those carried over by \code{\link{gravity_field}}, and
#'   \code{S} or \code{gravity}).  Node colours use the colour scale and the
#'   surface uses the fill scale, so both get a legend.  In a multi-panel field,
#'   panels whose graph lacks the attribute show it as missing.
#' @param surface \code{"interpolate"} (smooth Gaussian kernel interpolation of
#'   \eqn{S} with contour lines), \code{"voronoi"} (each grid cell takes its
#'   nearest node's \eqn{S}), or \code{"none"}.
#' @param alpha Opacity of the source-sink surface, from \code{0} (invisible)
#'   to \code{1} (opaque, the default).  Lower it to let a \code{base} map show
#'   through, e.g. \code{alpha = 0.5}; contour lines, edges, arrows and nodes
#'   stay opaque.
#' @param arrows Draw source-to-sink arrows (default \code{TRUE}).
#' @param palette Two colours, sink then source (default a colour-blind-safe
#'   blue/red pair).
#' @param subtitle_stats Add the degree share (\eqn{R^2} of \eqn{\Delta} on
#'   \eqn{\Delta^0}) and cor(\eqn{S}, degree) to each facet label (default
#'   \code{TRUE}).
#' @param mask_dist Surface cells farther than this from every node are left
#'   blank; default 1.2 times the median nearest-neighbour distance between
#'   nodes (floored at a quarter of the nodes' mean spacing, so near-coincident
#'   nodes do not blank the surface).
#' @param arrow_scale Arrow length multiplier (default 1).  The longest arrow
#'   over all facets is \code{arrow_scale} times the median edge length.
#' @param min_delta Hide arrows whose \eqn{|\Delta|} is below this quantile of
#'   all \eqn{|\Delta|} (default 0, show all).
#' @param grid_n Surface grid resolution per axis (default 140).
#' @return A \code{ggplot} object.
#' @seealso \code{\link{gravity_field}}, \code{\link{plot.popgraph}},
#'   \code{\link{source_sink_scores}}
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
#' f <- gravity_field(list(`local mean` = g, `degree-neutral` = g), gamma = c(1, 0.5))
#' plot(f, layout = xy)
#' @export
plot.gravity_field <- function(x, y, ...,
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
                               subtitle_stats = TRUE,
                               mask_dist = NULL,
                               arrow_scale = 1,
                               min_delta = 0,
                               grid_n = 140) {
  chkDots(...)
  .check_base(base)
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha < 0 || alpha > 1)
    stop("'alpha' must be a single number between 0 and 1.")
  if (!is.numeric(node_size)) node_size <- match.arg(node_size)
  node_labels <- match.arg(node_labels)
  edge_width  <- match.arg(edge_width)
  surface     <- match.arg(surface)
  parts <- .split_gravity_field(x)
  if (is.null(node_fill)) node_fill <- .resolve_node_fill(parts[[1]]$graph, NULL)
  fill_attr <- length(node_fill) == 1 &&
    any(vapply(parts, function(p) node_fill %in% igraph::vertex_attr_names(p$graph), logical(1)))
  if (!fill_attr && (length(node_fill) != 1 ||
                     inherits(try(grDevices::col2rgb(node_fill), silent = TRUE), "try-error")))
    stop("'node_fill' must be a single colour or the name of a vertex attribute.")
  diag <- x$diagnostics
  lab <- if (subtitle_stats) {
    st <- sprintf("degree share %.2f, cor(S, k) %.2f", diag$degree_share, diag$cor_S_degree)
    ifelse(nzchar(diag$panel), paste0(diag$panel, "\n", st), st)
  } else diag$panel
  lab <- make.unique(lab, sep = " ")

  # One layout for all panels when they share their populations.
  coords_for <- function(p) {
    .graph_layout(p$graph, layout, layout_graph = pgd(p$graph, gamma = p$diagnostics$gamma, output = "graph"),
                  nodes = p$nodes$Stratum)
  }
  same_nodes <- all(vapply(parts, function(p) setequal(p$nodes$Stratum, parts[[1]]$nodes$Stratum), logical(1)))
  shared <- if (same_nodes) coords_for(parts[[1]]) else NULL
  geo <- list(); nd <- list(); ed <- list(); seg <- list()
  for (i in seq_along(parts)) {
    p  <- parts[[i]]
    xy <- if (same_nodes) shared[p$nodes$Stratum, , drop = FALSE] else coords_for(p)
    geo[[i]] <- identical(attr(if (same_nodes) shared else xy, "source"), "geographic")
    # Only the computed columns: decorations (e.g. a 'fill' or 'alpha' vertex
    # attribute) must not reach the plotting canvas.
    n <- p$nodes[c("panel", "Stratum", "degree", "size", "S", "gravity")]
    n$node <- n$Stratum                      # the plotting canvas keys nodes by 'node'
    n$x <- xy[n$node, 1]; n$y <- xy[n$node, 2]; n$panel <- lab[i]
    # S is centred within each component, so the surface interpolates them apart.
    n$component <- igraph::components(p$graph)$membership[match(n$node, igraph::V(p$graph)$name)]
    # gravity_field() fills 'size' with degree when the graph has no size
    # attribute; mark it missing so node_size = "size" reports the fallback.
    if (!"size" %in% igraph::vertex_attr_names(p$graph)) n$size <- NA_real_
    if (fill_attr) {
      n <- .graph_node_fill(n, p$graph, node_fill)
      if (is.null(n$fill)) n$fill <- NA
    }
    e <- p$edges[c("panel", "from", "to", "weight", "delta", "source", "sink")]
    e <- .edge_midpoints(e, xy)
    e$panel <- lab[i]
    s <- .edge_segments(cbind(e$from, e$to), xy)
    s$weight <- e$weight; s$panel <- lab[i]
    nd[[i]] <- n; ed[[i]] <- e; seg[[i]] <- s
  }
  nd <- do.call(rbind, nd); ed <- do.call(rbind, ed); seg <- do.call(rbind, seg)
  nd$panel <- factor(nd$panel, lab); ed$panel <- factor(ed$panel, lab); seg$panel <- factor(seg$panel, lab)

  lim <- max(abs(nd$S), na.rm = TRUE); if (!is.finite(lim) || lim == 0) lim <- 1
  underlay <- .gravity_surface(nd, "S", lab, surface, mask_dist, grid_n, alpha)
  overlay <- list()
  if (arrows) {
    a <- ed[abs(ed$delta) > 1e-6, ]
    if (min_delta > 0 && nrow(a)) a <- a[abs(a$delta) >= stats::quantile(abs(ed$delta), min_delta), ]
    overlay <- .gravity_arrows(a, abs(a$delta), max(abs(ed$delta), na.rm = TRUE),
                               .median_edge_length(seg), arrow_scale)
  }

  p <- .graph_canvas(nd, seg, node_size = node_size, node_labels = node_labels,
                     node_fill = node_fill, edges = edges, edge_width = edge_width,
                     geographic = all(unlist(geo)), underlay = underlay, overlay = overlay,
                     base = base)
  p + ggplot2::scale_fill_gradient2(low = palette[1], mid = "#f7f7f7", high = palette[2], midpoint = 0,
                                    limits = c(-lim, lim),
                                    name = "Source\u2013sink score S\n(red = source, blue = sink)",
                                    na.value = NA)
}


# Interpolated (or Voronoi) surface of nd[[value]] over the node positions,
# one per panel, as underlay layers on the fill aesthetic.  With a
# 'component' column, each grid cell belongs to its nearest node's component
# and is interpolated from that component's nodes only.
#' @keywords internal
#' @noRd
.gravity_surface <- function(nd, value, lab, surface, mask_dist = NULL, grid_n = 140, alpha = 1) {
  if (surface == "none") return(list())
  xy1 <- nd[nd$panel == lab[1], c("x", "y")]
  dmat <- as.matrix(stats::dist(xy1)); diag(dmat) <- Inf
  nn <- stats::median(apply(dmat, 1, min))
  # Near-coincident nodes shrink the median nearest-neighbour distance toward
  # zero, which would mask almost the whole surface; floor it at a quarter of
  # the mean spacing of the nodes over their bounding box.
  box <- diff(range(xy1$x)) * diff(range(xy1$y))
  nn <- max(nn, 0.25 * sqrt(box / nrow(xy1)), 1e-9)
  if (is.null(mask_dist)) mask_dist <- 1.2 * nn

  rx <- range(nd$x); ry <- range(nd$y); pad <- 0.12 * max(diff(rx), diff(ry), 1e-9)
  gx <- seq(rx[1] - pad, rx[2] + pad, length.out = grid_n)
  gy <- seq(ry[1] - pad, ry[2] + pad, length.out = grid_n)
  gr <- expand.grid(x = gx, y = gy)
  surf <- do.call(rbind, lapply(lab, function(l) {
    n <- nd[nd$panel == l, ]
    d <- sqrt(outer(gr$x, n$x, "-")^2 + outer(gr$y, n$y, "-")^2)
    near <- apply(d, 1, which.min)
    v <- if (surface == "voronoi") {
      n[[value]][near]
    } else {
      h <- pmax(nn * 1.2, 1e-6)
      w <- exp(-0.5 * (d / h)^2)
      if (!is.null(n$component)) w <- w * outer(n$component[near], n$component, "==")
      as.numeric((w %*% n[[value]]) / pmax(rowSums(w), 1e-9))
    }
    v[apply(d, 1, min) > mask_dist] <- NA
    data.frame(gr, value = v, panel = l)
  }))
  surf$panel <- factor(surf$panel, lab)
  out <- list(ggplot2::geom_raster(data = surf, ggplot2::aes(.data$x, .data$y, fill = .data$value),
                                   interpolate = TRUE, na.rm = TRUE, alpha = alpha, inherit.aes = FALSE))
  if (surface == "interpolate")
    out[[2]] <- ggplot2::geom_contour(data = surf, ggplot2::aes(.data$x, .data$y, z = .data$value),
                                      colour = "grey35", linewidth = 0.25, bins = 10, na.rm = TRUE,
                                      inherit.aes = FALSE)
  out
}

# Arrows centred on each edge midpoint, pointing along (ux, uy), with length
# proportional to 'mag' (the largest, 'mag_max', is arrow_scale edge lengths).
# 'a' needs x_mid, y_mid, ux, uy.
#' @keywords internal
#' @noRd
.gravity_arrows <- function(a, mag, mag_max, edge_len, arrow_scale = 1, colour = "black",
                            legend = NULL) {
  if (!nrow(a) || !is.finite(mag_max) || mag_max <= 0) return(list())
  L <- arrow_scale * mag / mag_max * edge_len
  a$x0 <- a$x_mid - a$ux * L / 2; a$y0 <- a$y_mid - a$uy * L / 2
  a$x1 <- a$x_mid + a$ux * L / 2; a$y1 <- a$y_mid + a$uy * L / 2
  # With 'legend', the arrows get a linetype legend entry with that label.
  m <- ggplot2::aes(.data$x0, .data$y0, xend = .data$x1, yend = .data$y1)
  if (!is.null(legend)) {
    a$legend_key <- legend
    m <- ggplot2::aes(.data$x0, .data$y0, xend = .data$x1, yend = .data$y1, linetype = .data$legend_key)
  }
  list(ggplot2::geom_segment(data = a, m,
                             arrow = ggplot2::arrow(length = ggplot2::unit(0.16, "cm"), type = "closed"),
                             colour = colour, linewidth = 0.65, inherit.aes = FALSE))
}

# Midpoint and unit direction of each source -> sink edge, for .gravity_arrows().
#' @keywords internal
#' @noRd
.edge_midpoints <- function(e, xy) {
  sx <- xy[e$source, 1]; sy <- xy[e$source, 2]; tx <- xy[e$sink, 1]; ty <- xy[e$sink, 2]
  len <- sqrt((tx - sx)^2 + (ty - sy)^2); len[len == 0] <- 1
  e$x_mid <- (sx + tx) / 2; e$y_mid <- (sy + ty) / 2
  e$ux <- (tx - sx) / len; e$uy <- (ty - sy) / len
  e
}

#' @keywords internal
#' @noRd
.median_edge_length <- function(seg) {
  if (!nrow(seg)) return(1)
  stats::median(sqrt((seg$xend - seg$x)^2 + (seg$yend - seg$y)^2))
}
