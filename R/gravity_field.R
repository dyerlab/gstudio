#' Gravity field of a Population Graph: nodes and directed edge arrows with coordinates
#'
#' @description
#' Assembles everything needed to map genetic gravity: node coordinates, degree,
#' source-sink score \eqn{S} and gravity, and for every edge its asymmetry
#' \eqn{\Delta}, which endpoint is the source, the edge midpoint and the unit
#' vector pointing from source to sink.  Used by \code{\link{plot_gravity_field}}.
#'
#' @param graph An undirected \code{popgraph}/\code{igraph} with a numeric
#'   \code{weight} edge attribute.
#' @param coords Node coordinates, in order of preference: a two-column matrix or
#'   \code{data.frame} (rows named by node, or in vertex order; columns \code{x}/\code{y},
#'   \code{Longitude}/\code{Latitude}, or the first two numeric columns); the output of
#'   \code{\link{strata_coordinates}}; a character string specifying a layout algorithm
#'   (\code{"fr"} or \code{"kk"}); or \code{NULL} (default), in which case vertex
#'   attributes \code{x}/\code{y} or \code{Longitude}/\code{Latitude} are used if present,
#'   and otherwise defaults to a Kamada-Kawai layout with bi-directional pGD weights.
#' @param gamma Bandwidth multiplier (default \code{0.5}, degree-neutral; \code{1} local
#'   mean; \code{Inf} degree-only).  See \code{\link{source_sink_scores}}.
#' @param layout Optional layout specification overriding or complementing \code{coords}:
#'   \code{"kk"} (Kamada-Kawai, default), \code{"fr"} (Fruchterman-Reingold), or a layout function.
#'   When a layout is computed, bi-directional arc weights from \code{\link{pgd}} are used
#'   to let directional gene flow shape the topology.
#' @return An object of class \code{"gravity_field"}: a list with
#'   \describe{
#'     \item{\code{nodes}}{\code{data.frame}: \code{node}, \code{x}, \code{y}, \code{degree},
#'       \code{size} (vertex \code{size} attribute if present, else \code{degree}),
#'       \code{S} (source-sink score), \code{gravity}.}
#'     \item{\code{edges}}{\code{data.frame}: \code{from}, \code{to}, \code{weight},
#'       \code{delta} (\eqn{\Delta_{from \to to}}), \code{source}, \code{sink},
#'       \code{x_mid}, \code{y_mid}, and the unit vector \code{ux}, \code{uy} from source
#'       to sink.}
#'     \item{\code{gamma}}{The bandwidth multiplier used.}
#'     \item{\code{diagnostics}}{Named numeric: \code{degree_share} (\eqn{R^2} of
#'       \eqn{\Delta} on \eqn{\Delta^0}), \code{cor_S_degree}, \code{cor_S_S0},
#'       \code{gradient_share} (see \code{\link{source_sink_scores}}).}
#'     \item{\code{coords_source}}{Where the coordinates came from: \code{"supplied"},
#'       \code{"vertex attributes"} or \code{"layout"} (not geographic).}
#'   }
#'   Methods: \code{print()} summarizes the field (bandwidth, size, degree diagnostics and
#'   the strongest sources and sinks); \code{plot()} draws it with
#'   \code{\link{plot_gravity_field}}; \code{as.data.frame()} returns the node or edge table.
#' @seealso \code{\link{plot_gravity_field}}, \code{\link{source_sink_scores}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' library(igraph)
#' A <- matrix(0, 5, 5, dimnames = list(LETTERS[1:5], LETTERS[1:5]))
#' A["A", "B"] <- A["B", "A"] <- 1; A["B", "C"] <- A["C", "B"] <- 1.5
#' A["C", "D"] <- A["D", "C"] <- 0.8; A["D", "E"] <- A["E", "D"] <- 2
#' A["B", "D"] <- A["D", "B"] <- 2.5
#' g <- graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE)
#' f <- gravity_field(g, coords = cbind(x = 1:5, y = c(0, 1, 0, 1, 0)))
#' f
#' head(as.data.frame(f, what = "edges"))
#' plot(f)
#' @export
gravity_field <- function(graph, coords = NULL, gamma = 0.5, layout = NULL) {
  graph <- .gravity_check(graph)
  nodes <- igraph::V(graph)$name
  xy <- .gravity_coords(graph, coords, nodes, gamma = gamma, layout = layout); src_coords <- attr(xy, "source")
  sc <- source_sink_scores(graph, gamma)
  ed <- gravity_edges(graph, gamma)
  src <- ifelse(ed$Delta >= 0, ed$from, ed$to); snk <- ifelse(ed$Delta >= 0, ed$to, ed$from)
  sx <- xy[src, 1]; sy <- xy[src, 2]; tx <- xy[snk, 1]; ty <- xy[snk, 2]
  len <- sqrt((tx - sx)^2 + (ty - sy)^2); len[len == 0] <- 1
  sz <- if ("size" %in% igraph::vertex_attr_names(graph)) igraph::V(graph)$size else sc$degree
  out <- list(
    nodes = data.frame(node = nodes, x = xy[nodes, 1], y = xy[nodes, 2], degree = sc$degree,
                       size = sz, S = sc$S, gravity = sc$gravity, row.names = NULL, stringsAsFactors = FALSE),
    edges = data.frame(from = ed$from, to = ed$to, weight = ed$weight, delta = ed$Delta, source = src, sink = snk,
                       x_mid = (xy[ed$from, 1] + xy[ed$to, 1]) / 2, y_mid = (xy[ed$from, 2] + xy[ed$to, 2]) / 2,
                       ux = (tx - sx) / len, uy = (ty - sy) / len, row.names = NULL, stringsAsFactors = FALSE))
  out$gamma <- gamma
  out$diagnostics <- c(degree_share = attr(sc, "degree_share"), cor_S_degree = attr(sc, "cor_S_degree"),
                       cor_S_S0 = attr(sc, "cor_S_S0"), gradient_share = attr(sc, "gradient_share"))
  out$coords_source <- src_coords
  class(out) <- "gravity_field"
  out
}

#' @rdname gravity_field
#' @param x A \code{gravity_field} object.
#' @param n Number of strongest sources and sinks to list (default 3).
#' @param ... Passed on (to \code{\link{plot_gravity_field}} for \code{plot()}).
#' @export
print.gravity_field <- function(x, n = 3, ...) {
  d <- x$diagnostics; nd <- x$nodes[order(-x$nodes$S), ]
  cat(sprintf("Gravity field: %d populations, %d edges, gamma = %g\n", nrow(x$nodes), nrow(x$edges), x$gamma))
  cat(sprintf("  degree share %.3f, cor(S, degree) %.3f, gradient share %.3f\n", d[["degree_share"]], d[["cor_S_degree"]], d[["gradient_share"]]))
  top <- function(v) if (nrow(v)) paste(sprintf("%s (%.3f)", utils::head(v$node, n), utils::head(v$S, n)), collapse = ", ") else "none"
  cat(sprintf("  strongest sources: %s\n", top(nd[nd$S > 0, ])))
  sk <- x$nodes[order(x$nodes$S), ]
  cat(sprintf("  strongest sinks:   %s\n", top(sk[sk$S < 0, ])))
  if (identical(x$coords_source, "layout")) cat("  coordinates: graph layout (not geographic)\n")
  invisible(x)
}

#' @rdname gravity_field
#' @param y Ignored.
#' @export
plot.gravity_field <- function(x, y, ...) plot_gravity_field(x, ...)

#' @rdname gravity_field
#' @param row.names,optional Ignored.
#' @param what \code{"nodes"} (default) or \code{"edges"}.
#' @export
as.data.frame.gravity_field <- function(x, row.names = NULL, optional = FALSE, what = c("nodes", "edges"), ...) {
  what <- match.arg(what)
  x[[what]]
}

#' @keywords internal
.gravity_coords <- function(graph, coords, nodes, gamma = 0.5, layout = NULL) {
  pick <- function(d) {
    d <- as.data.frame(d)
    nm <- names(d)
    if (all(c("x", "y") %in% nm)) m <- cbind(d$x, d$y)
    else if (all(c("Longitude", "Latitude") %in% nm)) m <- cbind(d$Longitude, d$Latitude)
    else { num <- vapply(d, is.numeric, logical(1)); if (sum(num) < 2) stop("'coords' needs two numeric columns"); m <- as.matrix(d[, which(num)[1:2]]) }
    key <- if ("Stratum" %in% nm) as.character(d$Stratum) else if (!is.null(rownames(d)) && all(nodes %in% rownames(d))) rownames(d) else NULL
    if (!is.null(key)) { if (!all(nodes %in% key)) stop("'coords' does not cover every node"); m <- m[match(nodes, key), , drop = FALSE] }
    else if (nrow(m) != length(nodes)) stop("'coords' must have one row per node (or be keyed by node)")
    m
  }

  calc_layout <- function(lay_type) {
    dg <- pgd(graph, gamma = gamma, output = "graph")
    old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
    set.seed(1)
    on.exit({ if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()) })
    if (is.function(lay_type)) {
      m <- lay_type(dg)
    } else {
      lay_str <- tolower(as.character(lay_type)[1])
      if (lay_str %in% c("kk", "kamada", "kamada-kawai", "kamada_kawai")) {
        m <- igraph::layout_with_kk(dg, weights = igraph::E(dg)$weight)
      } else {
        ew <- igraph::E(dg)$weight
        med_ew <- stats::median(ew)
        if (!is.finite(med_ew) || med_ew <= 0) med_ew <- 1
        w_attr <- 1 / (ew + med_ew * 0.25)
        m <- igraph::layout_with_fr(dg, weights = w_attr)
      }
    }
    m
  }

  src <- "supplied"
  if (!is.null(layout)) {
    src <- "layout"
    m <- calc_layout(layout)
  } else if (is.character(coords) && length(coords) == 1L && tolower(coords) %in% c("fr", "fruchterman", "kk", "kamada", "kamada-kawai", "kamada_kawai")) {
    src <- "layout"
    m <- calc_layout(coords)
  } else if (!is.null(coords)) {
    m <- pick(coords)
  } else {
    va <- igraph::vertex_attr_names(graph)
    if (all(c("x", "y") %in% va)) {
      src <- "vertex attributes"
      m <- cbind(igraph::V(graph)$x, igraph::V(graph)$y)
    } else if (all(c("Longitude", "Latitude") %in% va)) {
      src <- "vertex attributes"
      m <- cbind(igraph::V(graph)$Longitude, igraph::V(graph)$Latitude)
    } else {
      src <- "layout"
      message("No coordinates supplied: using a Kamada-Kawai layout (positions are not geographic).")
      m <- calc_layout("kk")
    }
  }
  m <- matrix(as.numeric(m), ncol = 2, dimnames = list(nodes, c("x", "y")))
  attr(m, "source") <- src
  m
}
