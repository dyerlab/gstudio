#' Plot a Population Graph
#'
#' @description
#' Draws a Population Graph with ggplot2: edges as segments, nodes as badges
#' sized by their \code{size} attribute (for graphs from \code{\link{popgraph}},
#' within-population genetic variability; degree when there is none), and node
#' labels.  Directed
#' graphs, such as those from \code{\link{asymmetric_popgraph}}, are drawn with
#' arrows, and reciprocal arcs are offset so both directions stay visible.
#'
#' \code{plot()} on a Population Graph and on a \code{\link{gravity_field}} share
#' one plotting backend, so \code{layout}, \code{node_size}, \code{node_labels},
#' \code{node_fill}, \code{edges} and \code{edge_width} work the same way on
#' both.  The result is a \code{ggplot} object that can be extended with
#' \code{+}.  For full control, build the plot yourself with \pkg{ggraph}; the
#' igraph base-graphics method is still available as
#' \code{igraph::plot.igraph()}.
#'
#' @param x A \code{popgraph} object.
#' @param y Ignored.
#' @param ... Not used; any argument passed here is reported, since the
#'   plotting options below must be named.
#' @param layout How to place the nodes.  \code{NULL} (default) uses vertex
#'   attributes \code{x}/\code{y} or \code{Longitude}/\code{Latitude} when
#'   present (for example from \code{\link{decorate_graph}}) and otherwise a
#'   Kamada-Kawai layout, which spreads nodes apart well.  Otherwise one of: a layout name
#'   (\code{"fr"}, \code{"kk"}, \code{"circle"} or \code{"mds"}); a layout
#'   function taking an \code{igraph} and returning a two-column matrix; a
#'   two-column matrix (rows named by node, or in vertex order); or a
#'   \code{data.frame} with columns \code{name}/\code{x}/\code{y}, the output
#'   of \code{\link{strata_coordinates}}, or two numeric columns.  Named
#'   layouts use the edge weights (floored at a quarter of their median) and a
#'   fixed seed, so the same graph always gets the same picture.
#' @param node_size \code{"size"} (default: the vertex \code{size} attribute,
#'   falling back to degree when the graph has none), \code{"degree"},
#'   \code{"constant"}, or a single number giving a fixed point size.  For a
#'   graph from \code{\link{popgraph}}, \code{size} is each population's
#'   within-population genetic standard deviation, rescaled to start at 5 (Dyer &
#'   Nason 2004); it is not the number of individuals sampled.
#' @param node_labels \code{"name"} (default), \code{"degree"}, \code{"size"},
#'   or \code{"none"}.  Labels are drawn just above each node; add your own
#'   label layer (e.g. \code{ggrepel::geom_text_repel()}) for crowded graphs.
#' @param node_fill \code{NULL} (default) fills nodes by a vertex attribute
#'   named \code{region} (any capitalisation, e.g. \code{Region} from
#'   \code{\link{decorate_graph}}) when the graph has one, and white otherwise.
#'   Otherwise a colour for every node, or the name of a vertex attribute
#'   (categorical or continuous) to map.  Mapped attributes use the
#'   \strong{colour} scale (default viridis), so restyle them with
#'   \code{scale_colour_*()}, e.g. \code{+ scale_colour_brewer(palette =
#'   "Set2")}; the fill scale is left free (a gravity field uses it for
#'   \eqn{S}).
#' @param edges Draw the edges (default \code{TRUE}).
#' @param edge_width \code{"constant"} (default) or \code{"weight"} to scale
#'   edge width by the \code{weight} attribute.
#' @param base An optional \code{ggplot} to draw the graph on, so that layers
#'   can go \emph{underneath} it, e.g. \code{ggplot() + geom_sf(data = map)} or
#'   \code{ggplot() + geom_raster(aes(x, y, fill = elevation), data = dem)}.
#'   The graph's layers are added on top and do not inherit the base plot's
#'   mappings.  If the base sets a coordinate system (e.g. \code{coord_sf()},
#'   which \code{geom_sf()} adds) it is kept, so the base layers must use the
#'   same coordinates as the layout (longitude/latitude for a geographic
#'   layout).  Node colours use the colour scale (and a gravity surface the fill
#'   scale), so a base layer mapped to the same scale will be replaced; set
#'   such layers' colours directly instead.  The base plot keeps its own
#'   theme and axis labels.
#' @return A \code{ggplot} object.
#' @seealso \code{\link{plot.gravity_field}}, \code{\link{animate_popgraphs}},
#'   \code{\link{decorate_graph}}, \code{\link{asymmetric_popgraph}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' data(lopho)
#' plot(lopho)
#' \donttest{
#' data(baja)
#' lopho_dec <- decorate_graph(lopho, baja, stratum = "Population")
#' plot(lopho_dec, edge_width = "weight")          # filled by Region, sized by size
#' plot(asymmetric_popgraph(lopho_dec), node_labels = "none")
#'
#' # Layers underneath the graph: pass them in as 'base'
#' library(ggplot2)
#' coast <- data.frame(x = c(-115, -109, -109, -115), y = c(23, 23, 31, 31))
#' under <- ggplot() + geom_polygon(aes(x, y), data = coast, fill = "grey92")
#' plot(lopho_dec, base = under)
#' }
#' @export
plot.popgraph <- function(x, y, ...,
                          layout = NULL,
                          node_size = c("size", "degree", "constant"),
                          node_labels = c("name", "degree", "size", "none"),
                          node_fill = NULL,
                          edges = TRUE,
                          edge_width = c("constant", "weight"),
                          base = NULL) {
  chkDots(...)
  .check_base(base)
  if (!is.numeric(node_size)) node_size <- match.arg(node_size)
  node_labels <- match.arg(node_labels)
  edge_width  <- match.arg(edge_width)

  g <- igraph::upgrade_graph(x)
  if (is.null(igraph::V(g)$name))
    igraph::V(g)$name <- as.character(seq_len(igraph::vcount(g)))
  directed <- igraph::is_directed(g)
  if (edge_width == "weight" && is.null(igraph::E(g)$weight))
    stop("edge_width = \"weight\" needs a 'weight' edge attribute.")

  xy <- .graph_layout(g, layout)
  nd <- .graph_nodes(g, xy)
  node_fill <- .resolve_node_fill(g, node_fill)
  nd <- .graph_node_fill(nd, g, node_fill)

  el <- igraph::as_edgelist(g, names = TRUE)
  ed <- .edge_segments(el, xy, directed = directed)
  ed$weight <- if (is.null(igraph::E(g)$weight)) 1 else igraph::E(g)$weight
  ed$panel  <- nd$panel[1]

  .graph_canvas(nd, ed, node_size = node_size, node_labels = node_labels,
                node_fill = node_fill, edges = edges, edge_width = edge_width,
                arrows = directed, geographic = identical(attr(xy, "source"), "geographic"),
                base = base)
}


# ---------------------------------------------------------------------------
# Shared backend for plot.popgraph(), plot.gravity_field() and
# animate_popgraphs().
# ---------------------------------------------------------------------------

# Resolve 'layout' into a node-by-2 coordinate matrix (rows named by node).
# 'layout_graph' is the graph that named or function layouts are computed on
# (e.g. the pGD graph for a gravity field, or the union graph of an animation).
# attr(, "source") is "geographic", "attributes", "supplied" or "layout".
#' @keywords internal
#' @noRd
.graph_layout <- function(graph, layout = NULL, layout_graph = graph,
                          nodes = igraph::V(graph)$name) {
  src <- "layout"
  if (is.null(layout)) {
    va <- igraph::vertex_attr_names(graph)
    if (all(c("x", "y") %in% va)) {
      m <- cbind(igraph::V(graph)$x, igraph::V(graph)$y)
      rownames(m) <- igraph::V(graph)$name
      m <- m[nodes, , drop = FALSE]
      src <- "attributes"
    } else if (all(c("Longitude", "Latitude") %in% va)) {
      m <- cbind(igraph::V(graph)$Longitude, igraph::V(graph)$Latitude)
      rownames(m) <- igraph::V(graph)$name
      m <- m[nodes, , drop = FALSE]
      src <- "geographic"
    } else {
      layout <- "kk"
    }
  }
  if (!is.null(layout)) {
    if (is.character(layout)) {
      if (length(layout) != 1)
        stop("'layout' must be a single layout name.")
      layout <- .layout_function(layout)
    }
    if (is.function(layout)) {
      m <- .with_seed(layout(layout_graph))
      if (!is.matrix(m) || ncol(m) < 2 || nrow(m) != igraph::vcount(layout_graph))
        stop("A layout function must return a two-column matrix with one row per node.")
      rownames(m) <- igraph::V(layout_graph)$name
      missing_nodes <- setdiff(nodes, rownames(m))
      if (length(missing_nodes))
        stop("The layout graph has no coordinates for: ", paste(missing_nodes, collapse = ", "))
      m <- m[nodes, 1:2, drop = FALSE]
    } else if (is.matrix(layout) || is.data.frame(layout)) {
      m <- .coords_from_table(layout, nodes)
      src <- attr(m, "source")
    } else {
      stop("'layout' must be NULL, a layout name, a layout function, or a ",
           "matrix/data.frame of coordinates.")
    }
  }
  m <- matrix(as.numeric(m), ncol = 2, dimnames = list(nodes, c("x", "y")))
  if (anyNA(m))
    stop("The layout has missing coordinates for: ",
         paste(nodes[!stats::complete.cases(m)], collapse = ", "))
  attr(m, "source") <- src
  m
}

# Named layouts.  Edge weights are distances, so they are floored at a quarter
# of their median: Kamada-Kawai uses them as target distances and
# Fruchterman-Reingold uses their reciprocal as attraction.  Without the floor,
# near-zero weights collapse node pairs onto the same point.
#' @keywords internal
#' @noRd
.layout_function <- function(name) {
  floored <- function(g) {
    w <- igraph::E(g)$weight
    if (is.null(w)) return(NULL)
    med <- stats::median(w)
    if (!is.finite(med) || med <= 0) med <- 1
    w + 0.25 * med
  }
  switch(tolower(name),
         fr = , fruchterman = , `fruchterman-reingold` = function(g) {
           w <- floored(g)
           igraph::layout_with_fr(g, weights = if (is.null(w)) NA else 1 / w)
         },
         kk = , kamada = , `kamada-kawai` = , kamada_kawai = function(g) {
           w <- floored(g)
           igraph::layout_with_kk(g, weights = if (is.null(w)) NA else w)
         },
         circle = function(g) igraph::layout_in_circle(g),
         mds    = function(g) igraph::layout_with_mds(g),
         stop("Unknown layout '", name, "'. Use one of 'fr', 'kk', 'circle', 'mds', ",
              "a layout function, or a matrix/data.frame of coordinates."))
}

# Evaluate a stochastic layout with a fixed seed, leaving the caller's RNG
# state as it was.
#' @keywords internal
#' @noRd
.with_seed <- function(expr, seed = 1L) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit({
    if (had) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv())
  })
  set.seed(seed)
  expr
}

# Coordinates from a matrix or data.frame, keyed by node where possible.
#' @keywords internal
#' @noRd
.coords_from_table <- function(d, nodes) {
  src <- "supplied"
  key <- NULL
  if (is.matrix(d)) {
    if (ncol(d) < 2) stop("A layout matrix must have two columns.")
    m <- d[, 1:2, drop = FALSE]
    key <- rownames(d)
  } else {
    nm <- names(d)
    if (all(c("x", "y") %in% nm)) {
      m <- cbind(d$x, d$y)
    } else if (all(c("Longitude", "Latitude") %in% nm)) {
      m <- cbind(d$Longitude, d$Latitude)
      src <- "geographic"
    } else {
      num <- vapply(d, is.numeric, logical(1))
      if (sum(num) < 2) stop("A layout data.frame needs two numeric coordinate columns.")
      m <- as.matrix(d[, which(num)[1:2]])
    }
    key <- if ("name" %in% nm) as.character(d$name)
           else if ("Stratum" %in% nm) as.character(d$Stratum)
           else if ("node" %in% nm) as.character(d$node)
           else if (!is.null(rownames(d)) && all(nodes %in% rownames(d))) rownames(d)
           else NULL
  }
  if (!is.null(key)) {
    missing_nodes <- setdiff(nodes, key)
    if (length(missing_nodes))
      stop("The supplied layout has no coordinates for: ", paste(missing_nodes, collapse = ", "))
    m <- m[match(nodes, key), , drop = FALSE]
  } else if (nrow(m) != length(nodes)) {
    stop("The supplied layout must have one row per node, or be keyed by node name.")
  }
  attr(m, "source") <- src
  m
}

# Node table for the canvas: node, x, y, degree, size, panel.
#' @keywords internal
#' @noRd
.graph_nodes <- function(graph, xy, panel = "") {
  nodes <- rownames(xy)
  und <- if (igraph::is_directed(graph))
    igraph::as_undirected(graph, mode = "collapse") else graph
  deg <- igraph::degree(und)[nodes]
  deg[is.na(deg)] <- 0
  sz <- if ("size" %in% igraph::vertex_attr_names(graph))
    igraph::V(graph)$size[match(nodes, igraph::V(graph)$name)] else NA_real_
  data.frame(node = nodes, x = unname(xy[, 1]), y = unname(xy[, 2]), degree = unname(as.numeric(deg)),
             size = sz, panel = panel, row.names = NULL, stringsAsFactors = FALSE)
}

# Default fill: a vertex attribute named "region" (any case) if the graph has
# one, otherwise white.
#' @keywords internal
#' @noRd
.resolve_node_fill <- function(graph, node_fill) {
  if (!is.null(node_fill)) return(node_fill)
  va <- igraph::vertex_attr_names(graph)
  reg <- va[tolower(va) == "region"]
  if (length(reg)) reg[1] else "white"
}

# Attach the vertex attribute named by 'node_fill' (if it is one) as nd$fill.
#' @keywords internal
#' @noRd
.graph_node_fill <- function(nd, graph, node_fill) {
  if (length(node_fill) == 1 && node_fill %in% igraph::vertex_attr_names(graph)) {
    v <- igraph::vertex_attr(graph, node_fill)
    nd$fill <- v[match(nd$node, igraph::V(graph)$name)]
  }
  nd
}

# Segment table for an edge list.  Directed arcs are shortened so arrowheads
# stop short of the node badges, and reciprocal arcs are offset to either side.
#' @keywords internal
#' @noRd
.edge_segments <- function(el, xy, directed = FALSE) {
  x0 <- xy[el[, 1], 1]; y0 <- xy[el[, 1], 2]
  x1 <- xy[el[, 2], 1]; y1 <- xy[el[, 2], 2]
  if (directed && nrow(el)) {
    ext <- max(diff(range(xy[, 1])), diff(range(xy[, 2])), 1e-9)
    len <- sqrt((x1 - x0)^2 + (y1 - y0)^2); len[len == 0] <- 1
    ux <- (x1 - x0) / len; uy <- (y1 - y0) / len
    cut <- pmin(0.03 * ext, 0.3 * len)
    rev_pair <- paste(el[, 2], el[, 1]) %in% paste(el[, 1], el[, 2])
    off <- ifelse(rev_pair, 0.012 * ext, 0)
    nx <- uy; ny <- -ux                      # right-hand normal
    x0 <- x0 + ux * cut + nx * off; y0 <- y0 + uy * cut + ny * off
    x1 <- x1 - ux * cut + nx * off; y1 <- y1 - uy * cut + ny * off
  }
  data.frame(from = el[, 1], to = el[, 2], x = unname(x0), y = unname(y0),
             xend = unname(x1), yend = unname(y1), stringsAsFactors = FALSE)
}

# Resolve node_size into nd$plot_size and how to scale it.
#' @keywords internal
#' @noRd
.graph_node_size <- function(nd, node_size) {
  if (is.numeric(node_size))
    return(list(nd = transform(nd, plot_size = node_size[1]), scale = FALSE, title = NULL))
  if (identical(node_size, "constant"))
    return(list(nd = transform(nd, plot_size = 4.5), scale = FALSE, title = NULL))
  if (identical(node_size, "size")) {
    # The node's own size (within-population genetic variability for a
    # popgraph()) is more informative than degree; fall back to degree
    # only when the graph carries no size attribute.
    has_size <- !all(is.na(nd$size))
    nd$plot_size <- if (has_size) nd$size else nd$degree
    return(list(nd = nd, scale = TRUE, title = if (has_size) "Size" else "Degree"))
  }
  nd$plot_size <- nd$degree
  list(nd = nd, scale = TRUE, title = "Degree")
}

# Build the plot.  Layer order: underlay (e.g. a gravity surface), edges,
# overlay (e.g. gravity arrows), nodes, labels.  nd$panel / ed$panel are
# factors; the plot is faceted when any panel has a non-empty name.
# 'node_colours' optionally gives named colours for a categorical node_fill.
# 'base' is a ggplot to draw on (checked by .check_base()); the graph's layers
# do not inherit its mappings, and its coordinate system (if any), theme and
# axis labels are kept.
#' @keywords internal
#' @noRd
.graph_canvas <- function(nd, ed, node_size = "degree", node_labels = "name",
                          node_fill = "white", edges = TRUE,
                          edge_width = "constant", arrows = FALSE,
                          geographic = FALSE, underlay = list(),
                          overlay = list(), limits = NULL, node_colours = NULL,
                          base = NULL) {
  if (!is.factor(nd$panel)) nd$panel <- factor(nd$panel, unique(nd$panel))
  if (nrow(ed)) ed$panel <- factor(ed$panel, levels(nd$panel))
  if (is.null(nd$alpha)) nd$alpha <- 1

  p <- if (is.null(base)) ggplot2::ggplot() else base
  for (l in underlay) p <- p + l

  if (edges && nrow(ed)) {
    arr <- if (arrows) ggplot2::arrow(length = ggplot2::unit(0.16, "cm"), type = "closed") else NULL
    if (identical(edge_width, "weight")) {
      p <- p + ggplot2::geom_segment(data = ed, ggplot2::aes(.data$x, .data$y, xend = .data$xend,
                                                             yend = .data$yend, linewidth = .data$weight),
                                     colour = "grey40", alpha = 0.7, arrow = arr, inherit.aes = FALSE) +
        ggplot2::scale_linewidth_continuous(range = c(0.2, 1.6), name = "Edge weight")
    } else {
      p <- p + ggplot2::geom_segment(data = ed, ggplot2::aes(.data$x, .data$y, xend = .data$xend,
                                                             yend = .data$yend),
                                     colour = "grey40", linewidth = 0.5, alpha = 0.7, arrow = arr,
                                     inherit.aes = FALSE)
    }
  }

  for (l in overlay) p <- p + l

  ns <- .graph_node_size(nd, node_size); nd <- ns$nd
  # Nodes are a solid dot coloured by node_fill under a black ring.  The
  # colour comes from the colour scale, not fill, so the fill scale stays free
  # for underlays such as the gravity surface.
  map_fill <- !is.null(nd$fill)
  m <- list(x = quote(.data$x), y = quote(.data$y), alpha = quote(.data$alpha))
  if (ns$scale) m$size <- quote(.data$plot_size)
  aes_ring <- do.call(ggplot2::aes, m)
  if (map_fill) m$colour <- quote(.data$fill)
  aes_dot  <- do.call(ggplot2::aes, m)
  # The size legend shows the ring only; the colour legend shows the dot.
  dot  <- list(data = nd, mapping = aes_dot, shape = 16, show.legend = c(size = FALSE),
               inherit.aes = FALSE)
  ring <- list(data = nd, mapping = aes_ring, shape = 21, fill = NA,
               colour = "black", stroke = 0.8, inherit.aes = FALSE)
  if (!map_fill) dot$colour <- node_fill
  if (!ns$scale) dot$size <- ring$size <- nd$plot_size[1]
  p <- p + do.call(ggplot2::geom_point, dot) + do.call(ggplot2::geom_point, ring) +
    ggplot2::scale_alpha_identity()
  if (ns$scale)
    p <- p + ggplot2::scale_size_continuous(range = c(2.8, 6.5), name = ns$title)
  if (map_fill)
    p <- p + if (is.numeric(nd$fill)) ggplot2::scale_colour_viridis_c(name = node_fill)
             else if (!is.null(node_colours))
               ggplot2::scale_colour_manual(name = node_fill, values = node_colours,
                                            guide = ggplot2::guide_legend(override.aes = list(size = 4)))
             else ggplot2::scale_colour_viridis_d(name = node_fill,
                                                  guide = ggplot2::guide_legend(override.aes = list(size = 4)))

  if (node_labels != "none") {
    nd$node_label <- switch(node_labels,
                            degree = format(nd$degree, trim = TRUE),
                            size   = format(round(nd$size, 1), trim = TRUE),
                            nd$node)
    # Plain text, not ggrepel: repel layout run while an on-screen device
    # (e.g. macOS Quartz) is still opening fails with a grid "depth" error.
    p <- p + ggplot2::geom_text(data = nd, ggplot2::aes(.data$x, .data$y, label = .data$node_label,
                                                        alpha = .data$alpha),
                                size = 2.6, fontface = "bold", colour = "grey15", vjust = -1.6,
                                inherit.aes = FALSE)
  }

  if (any(nzchar(levels(nd$panel))))
    p <- p + ggplot2::facet_wrap(~panel)
  # Keep a coordinate system the base plot sets (e.g. coord_sf() for geom_sf()).
  if (is.null(base) || isTRUE(base$coordinates$default))
    p <- p + if (geographic)
    ggplot2::coord_quickmap(xlim = limits$x, ylim = limits$y, expand = is.null(limits))
  else
    ggplot2::coord_equal(xlim = limits$x, ylim = limits$y, expand = is.null(limits))
  # A base plot keeps its own theme and axis labels.
  if (!is.null(base)) return(p)
  p + ggplot2::labs(x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())
}

#' @keywords internal
#' @noRd
.check_base <- function(base) {
  if (!is.null(base) && !ggplot2::is_ggplot(base))
    stop("'base' must be NULL or a ggplot, e.g. ggplot() + geom_sf(data = map).")
  base
}
