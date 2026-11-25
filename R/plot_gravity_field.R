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
#' @section With and without the topology:
#' By default the graph is drawn over the surface.  \code{show_graph = FALSE}
#' draws the surface alone, with the same contours, component boundaries and
#' colour scale, so the two versions can be shown side by side;
#' \code{graph_alpha} gives the steps in between.
#'
#' @section Cropping the surface:
#' The surface is first limited by \code{mask} and \code{mask_dist}.  With a
#' \code{base} map, \code{clip_to_base = TRUE} then blanks the cells outside
#' the map's polygons, so that a surface over land leaves the water empty.  The
#' cells are tested at their centres, so raise \code{grid_n} (e.g. 400) to
#' follow a coastline closely.
#' \code{\link{gravity_surface}} gives the same cropped surface as a table or
#' raster, with the polygons given as \code{clip}.
#'
#' @inheritParams plot.popgraph
#' @param x A \code{\link{gravity_field}}.
#' @param layout How to place the nodes; see \code{\link{plot.popgraph}}.  Named
#'   and function layouts are computed on the bi-directional \code{\link{dcgd}}
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
#' @param mask Where the surface is drawn; see \code{\link{gravity_surface}}.
#'   \code{"hull"} (default) fills the convex hull of each connected component,
#'   with a margin of \code{mask_dist} around it; \code{"nodes"} draws a disc of
#'   radius \code{mask_dist} around each node.
#' @param mask_dist Radius of the disc around each node, and so the margin
#'   around each hull, beyond which the surface is left blank; default
#'   1.2 times the median nearest-neighbour distance between nodes (floored at
#'   a quarter of the nodes' mean spacing, so near-coincident nodes do not
#'   blank the surface).
#' @param boundary Draw a grey line where the surfaces of two connected components
#'   meet (default \code{TRUE}).  \eqn{S} is comparable only within a component,
#'   so colours on the two sides of the line are not comparable.
#' @param clip_to_base With a \code{base} map, also blank the surface outside
#'   the base plot's polygons (its \code{geom_polygon()} and \code{geom_sf()}
#'   layers), so that, for example, a surface over land does not spill into the
#'   sea (default \code{FALSE}).  Needs a geographic layout (node
#'   coordinates in the base map's longitude and latitude).
#' @param show_graph Draw the graph (edges, arrows, nodes and labels) on top of
#'   the surface (default \code{TRUE}); \code{FALSE} draws the surface alone.
#' @param graph_alpha Opacity of the graph drawn over the surface, from
#'   \code{0} (hidden) to \code{1} (default, fully drawn).  A single number
#'   applies to the whole graph: an intermediate value, e.g. \code{0.35}, keeps
#'   the graph visible but lets the surface dominate.  A named vector sets the
#'   parts separately, with any part not named left at \code{1}: \code{nodes},
#'   \code{edges}, \code{arrows} and \code{labels}, e.g.
#'   \code{c(nodes = 0.4, edges = 0.2, arrows = 0)}.  Edges are drawn at 0.7
#'   times their value.  All zero is the same as \code{show_graph = FALSE}.
#'   To draw your own labels (e.g. \code{ggrepel::geom_text_repel()}) set
#'   \code{node_labels = "none"} and take the node positions from
#'   \code{\link{plot_nodes}}.
#' @param arrow_scale Arrow length multiplier (default 1).  The longest arrow
#'   over all facets is \code{arrow_scale} times the median edge length.
#' @param min_delta Hide arrows whose \eqn{|\Delta|} is below this quantile of
#'   all \eqn{|\Delta|} (default 0, show all).
#' @param grid_n Surface grid resolution per axis (default 140).  A finer grid
#'   follows a clipped coastline more closely.
#' @return A \code{ggplot} object.
#' @seealso \code{\link{gravity_surface}} for the surface as a table or raster,
#'   \code{\link{gravity_field}}, \code{\link{plot.popgraph}},
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
#' plot(f, layout = xy, show_graph = FALSE)       # the surface alone
#' plot(f, layout = xy, graph_alpha = c(nodes = 0.5, edges = 0.2, arrows = 0))
#'
#' # On a map, with the surface cropped to land: with and without the topology
#' library( ggplot2 )
#' V( g )$Longitude <- seq( -114, -110, length.out = K )
#' V( g )$Latitude  <- seq( 29, 24, length.out = K ) + 0.3 * sin( 1:K )
#' coast <- data.frame( x = c( -115, -113.4, -109.3, -110.3 ),
#'                      y = c( 29.5, 29.7, 23.9, 23.2 ) )
#' land <- ggplot() +
#'   geom_polygon( aes( x, y ), data = coast, fill = "grey85" ) +
#'   coord_quickmap()
#' field <- gravity_field( g )
#' plot( field, base = land, clip_to_base = TRUE )
#' plot( field, base = land, clip_to_base = TRUE, show_graph = FALSE )
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
                               grid_n = 140,
                               mask = c("hull", "nodes"),
                               boundary = TRUE,
                               clip_to_base = FALSE,
                               show_graph = TRUE,
                               graph_alpha = 1) {
  chkDots(...)
  .check_base(base)
  mask <- match.arg(mask)
  if (!is.logical(clip_to_base) || length(clip_to_base) != 1 || is.na(clip_to_base))
    stop("'clip_to_base' must be TRUE or FALSE.")
  if (!is.logical(boundary) || length(boundary) != 1 || is.na(boundary))
    stop("'boundary' must be TRUE or FALSE.")
  if (!is.logical(show_graph) || length(show_graph) != 1 || is.na(show_graph))
    stop("'show_graph' must be TRUE or FALSE.")
  ga <- .graph_alpha(graph_alpha)
  if (!show_graph) ga[] <- 0
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha < 0 || alpha > 1)
    stop("'alpha' must be a single number between 0 and 1.")
  .check_surface_grid(grid_n, mask_dist)
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

  placed <- .field_layouts(parts, layout)
  nd <- list(); ed <- list(); seg <- list()
  for (i in seq_along(parts)) {
    p  <- parts[[i]]
    xy <- placed$xy[[i]]
    # Only the computed columns: decorations (e.g. a 'fill' or 'alpha' vertex
    # attribute) must not reach the plotting canvas.
    n <- p$nodes[c("panel", "Stratum", "degree", "size", "S", "gravity")]
    n$node <- n$Stratum                      # the plotting canvas keys nodes by 'node'
    n$alpha <- ga[["nodes"]]; n$label_alpha <- ga[["labels"]]
    n$x <- xy[n$node, 1]; n$y <- xy[n$node, 2]; n$panel <- lab[i]
    # S is centred within each component, so the surface interpolates them apart.
    n$component <- .node_component(p)
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
  clip <- NULL
  if (clip_to_base) {
    if (is.null(base)) warning("'clip_to_base' needs a 'base' plot; the surface is not clipped.")
    else if (!placed$geographic) warning("'clip_to_base' needs a geographic layout; the surface is not clipped.")
    else clip <- .base_polygons(base)
  }
  underlay <- .gravity_surface(nd, "S", lab, surface, mask_dist, grid_n, alpha,
                               mask = mask, clip = clip,
                               boundary = boundary)
  overlay <- list()
  if (arrows && ga[["arrows"]] > 0) {
    a <- ed[abs(ed$delta) > 1e-6, ]
    if (min_delta > 0 && nrow(a)) a <- a[abs(a$delta) >= stats::quantile(abs(ed$delta), min_delta), ]
    overlay <- .gravity_arrows(a, abs(a$delta), max(abs(ed$delta), na.rm = TRUE),
                               .median_edge_length(seg), arrow_scale, alpha = ga[["arrows"]])
  }

  p <- .graph_canvas(nd, seg, node_size = node_size, node_labels = node_labels,
                     node_fill = node_fill, edges = edges && ga[["edges"]] > 0, edge_width = edge_width,
                     geographic = placed$geographic, underlay = underlay, overlay = overlay,
                     base = base, show_nodes = ga[["nodes"]] > 0, edge_alpha = 0.7 * ga[["edges"]],
                     show_labels = ga[["labels"]] > 0)
  p + ggplot2::scale_fill_gradient2(low = palette[1], mid = "#f7f7f7", high = palette[2], midpoint = 0,
                                    limits = c(-lim, lim),
                                    name = "Source\u2013sink score S\n(red = source, blue = sink)",
                                    na.value = NA)
}


# graph_alpha as a named vector over the graph's parts: one number for all of
# them, or a named subset with the rest at 1.
.graph_alpha <- function(a) {
  parts <- c("nodes", "edges", "arrows", "labels")
  if (!is.numeric(a) || !length(a) || anyNA(a) || any(a < 0 | a > 1))
    stop("'graph_alpha' must be numbers between 0 and 1.")
  if (is.null(names(a))) {
    if (length(a) != 1) stop("'graph_alpha' must be one number, or named by part (",
                             paste(parts, collapse = ", "), ").")
    return(stats::setNames(rep(a, length(parts)), parts))
  }
  bad <- setdiff(names(a), parts)
  if (length(bad) || anyDuplicated(names(a)))
    stop("'graph_alpha' names must be distinct parts among ", paste(parts, collapse = ", "),
         if (length(bad)) paste0("; not ", paste(bad, collapse = ", ")), ".")
  out <- stats::setNames(rep(1, length(parts)), parts)
  out[names(a)] <- a
  out
}


# The surface of nd[[value]] (see .surface_grid()) as underlay layers on the
# fill aesthetic.  Contours are drawn within each component, so none piles up
# where the surfaces of two components meet.
#' @keywords internal
#' @noRd
.gravity_surface <- function( nd, value, lab, surface, mask_dist = NULL, grid_n = 140, alpha = 1,
                              mask = "hull", clip = NULL, boundary = FALSE ) {
  if( surface == "none" ) return( list() )
  surf <- .surface_grid( nd, value, lab, surface, mask_dist, grid_n, mask = mask, clip = clip )
  surf$panel <- factor( surf$panel, lab )
  out <- list( ggplot2::geom_raster( data = surf,
                                     ggplot2::aes( .data$x, .data$y, fill = .data$value ),
                                     interpolate = TRUE,
                                     na.rm = TRUE,
                                     alpha = alpha,
                                     inherit.aes = FALSE ) )
  if( surface == "interpolate" ) {
    z   <- surf[!is.na( surf$value ), ]
    lev <- .contour_levels( z$value )
    # A component whose scores cross no level has no contours to draw.
    key   <- paste( z$panel, z$component )
    spans <- tapply( z$value, key, function( v ) any( lev > min( v ) & lev < max( v ) ) )
    z     <- z[as.logical( spans[key] ), ]
    if( nrow( z ) )
      out[[2]] <- ggplot2::geom_contour( data = z,
                                         ggplot2::aes( .data$x, .data$y,
                                                       z = .data$value,
                                                       group = .data$component ),
                                         colour = "grey35",
                                         linewidth = 0.25,
                                         breaks = lev,
                                         na.rm = TRUE,
                                         inherit.aes = FALSE )
  }
  if( boundary ) {
    b <- .surface_boundaries( surf )
    if( nrow( b ) )
      out[[length( out ) + 1]] <- ggplot2::geom_path( data = b,
                                                      ggplot2::aes( .data$x, .data$y,
                                                                    group = .data$line ),
                                                      colour = "grey35",
                                                      linewidth = 0.9,
                                                      lineend = "round",
                                                      inherit.aes = FALSE )
  }
  out
}

# Contour levels common to all components and panels: multiples of a tenth of
# the range, as geom_contour( bins = 10 ) places them.
#' @keywords internal
#' @noRd
.contour_levels <- function( v, bins = 10 ) {
  v <- v[is.finite( v )]
  if( !length( v ) ) return( numeric( 0 ) )
  r <- range( v )
  if( diff( r ) <= 0 ) return( numeric( 0 ) )
  levels_at <- function( w ) seq( floor( r[1] / w ), ceiling( r[2] / w ) ) * w
  lev <- levels_at( diff( r ) / ( bins - 1 ) )
  if( length( lev ) < bins + 1 ) lev <- levels_at( diff( r ) / bins )
  lev
}

# Arrows centred on each edge midpoint, pointing along (ux, uy), with length
# proportional to 'mag' (the largest, 'mag_max', is arrow_scale edge lengths).
# 'a' needs x_mid, y_mid, ux, uy.
#' @keywords internal
#' @noRd
.gravity_arrows <- function(a, mag, mag_max, edge_len, arrow_scale = 1, colour = "black",
                            legend = NULL, alpha = 1) {
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
                             colour = colour, linewidth = 0.65, alpha = alpha, inherit.aes = FALSE))
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

# Distance from each point (px, py) to the convex hull of the nodes (hx, hy):
# 0 inside, else the distance to the hull's boundary (a point or a segment for
# one or two distinct nodes).
#' @keywords internal
#' @noRd
.hull_distance <- function(px, py, hx, hy) {
  u <- unique(data.frame(x = hx, y = hy))
  if (nrow(u) == 1) return(sqrt((px - u$x)^2 + (py - u$y)^2))
  h <- if (nrow(u) == 2) seq_len(2) else grDevices::chull(u$x, u$y)
  vx <- u$x[h]; vy <- u$y[h]; m <- length(vx)
  seg <- if (m == 2) list(c(1, 2)) else lapply(seq_len(m), function(i) c(i, i %% m + 1))
  d <- Inf
  for (s in seg) {
    ax <- vx[s[1]]; ay <- vy[s[1]]; bx <- vx[s[2]]; by <- vy[s[2]]
    L2 <- (bx - ax)^2 + (by - ay)^2
    t <- if (L2 > 0) pmin(1, pmax(0, ((px - ax) * (bx - ax) + (py - ay) * (by - ay)) / L2)) else 0
    d <- pmin(d, sqrt((px - ax - t * (bx - ax))^2 + (py - ay - t * (by - ay))^2))
  }
  if (m >= 3) d[.in_rings(px, py, list(list(data.frame(x = vx, y = vy))))] <- 0
  d
}

# Polygons of a base plot, for clipping: a list of features, each a list of
# rings (data.frames x, y) combined by the even-odd rule, so holes are holes.
# geom_polygon() layers give one feature per group (rings by subgroup);
# geom_sf() layers give one feature per (multi)polygon, in longitude and
# latitude.  Other layers are ignored.
#' @keywords internal
#' @noRd
.base_polygons <- function(base) {
  built <- ggplot2::ggplot_build(base)
  out <- list()
  for (i in seq_along(base$layers)) {
    geom <- base$layers[[i]]$geom
    if (inherits(geom, "GeomSf")) {
      g <- built$data[[i]]$geometry
      if (is.null(g)) next
      if (!is.na(sf::st_crs(g)) && !isTRUE(sf::st_is_longlat(g))) g <- sf::st_transform(g, 4326)
      out <- c(out, .sfc_polygons(g))
    } else if (inherits(geom, "GeomPolygon")) {
      d <- built$data[[i]]
      if (!all(c("x", "y") %in% names(d))) next
      grp <- if (is.null(d$group)) rep(1L, nrow(d)) else d$group
      sub <- if (is.null(d$subgroup)) rep(1L, nrow(d)) else d$subgroup
      for (gi in split(seq_len(nrow(d)), grp))
        out[[length(out) + 1]] <- lapply(split(gi, sub[gi]), function(k) data.frame(x = d$x[k], y = d$y[k]))
    }
  }
  if (!length(out)) warning("'clip_to_base': the base plot has no polygon layers; the surface is not clipped.")
  if (length(out)) out else NULL
}

# Points inside any feature (even-odd over a feature's rings), by ray casting.
# Edges that no point's ray can cross are dropped first, and the rest are
# tested once for each distinct py: a grid has few of them, so a detailed
# coastline stays fast.
#' @keywords internal
#' @noRd
.in_rings <- function( px, py, features ) {
  inside <- logical( length( px ) )
  if( !length( px ) ) return( inside )
  x_min <- min( px )
  ry    <- range( py )
  rows  <- split( seq_along( py ), match( py, unique( py ) ) )
  for( f in features ) {
    e <- .ring_edges( f )
    e <- e[pmax( e$y0, e$y1 ) > ry[1] & pmin( e$y0, e$y1 ) <= ry[2] &
             pmax( e$x0, e$x1 ) >= x_min, ]
    if( !nrow( e ) ) next
    for( k in rows ) {
      y   <- py[k[1]]
      hit <- which( ( e$y1 > y ) != ( e$y0 > y ) )
      if( !length( hit ) ) next
      x1 <- e$x1[hit]
      y1 <- e$y1[hit]
      at <- sort( ( e$x0[hit] - x1 ) * ( y - y1 ) / ( e$y0[hit] - y1 ) + x1 )
      # Crossings to the right of each point; an odd number is inside.
      n_right   <- length( at ) - findInterval( px[k], at )
      inside[k] <- inside[k] | n_right %% 2 == 1
    }
  }
  inside
}

# The edges of all the rings of a feature, each from (x0, y0) to (x1, y1).
#' @keywords internal
#' @noRd
.ring_edges <- function( rings ) {
  e <- lapply( rings, function( r ) {
    r <- r[stats::complete.cases( r ), , drop = FALSE]
    m <- nrow( r )
    if( m < 3 ) return( NULL )
    prev <- c( m, seq_len( m - 1 ) )
    data.frame( x0 = r$x[prev], y0 = r$y[prev], x1 = r$x, y1 = r$y )
  } )
  e <- do.call( rbind, unname( e ) )
  if( is.null( e ) )
    e <- data.frame( x0 = numeric(), y0 = numeric(), x1 = numeric(), y1 = numeric() )
  e
}
