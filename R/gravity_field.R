#' Gravity field of one or more Population Graphs
#'
#' @description
#' Assembles everything needed to map genetic gravity: for every population its
#' degree, source-sink score \eqn{S} and gravity, and for every edge its
#' asymmetry \eqn{\Delta} and which endpoint is the source.  A field can hold
#' several panels (several graphs, or one graph at several bandwidths), which
#' \code{plot()} draws as facets on common colour and arrow scales.
#'
#' A field holds no coordinates: node placement is a drawing choice, made with
#' the \code{layout} argument of \code{\link{plot.gravity_field}}, which works
#' as it does for \code{\link{plot.popgraph}}.
#'
#' @param graph An undirected \code{popgraph}/\code{igraph} with a numeric
#'   \code{weight} edge attribute, or a named list of them (one panel each).
#' @param gamma Bandwidth multiplier (default \code{0.5}, degree-neutral;
#'   \code{1} local mean; \code{Inf} degree-only), recycled over the panels.
#'   See \code{\link{source_sink_scores}}.  Giving one graph and several values,
#'   e.g. \code{gamma = c(1, 0.5)}, makes one panel per value.
#' @return An object of class \code{"gravity_field"}: a list with
#'   \describe{
#'     \item{\code{nodes}}{\code{data.frame}: \code{panel}, \code{Stratum},
#'       \code{degree}, \code{size} (vertex \code{size} attribute if present,
#'       else \code{degree}), \code{S} (source-sink score), \code{gravity}.}
#'     \item{\code{edges}}{\code{data.frame}: \code{panel}, \code{from}, \code{to},
#'       \code{weight}, \code{delta} (\eqn{\Delta_{from \to to}}), \code{source},
#'       \code{sink}.}
#'     \item{\code{diagnostics}}{\code{data.frame}, one row per panel:
#'       \code{panel}, \code{gamma}, \code{degree_share} (\eqn{R^2} of \eqn{\Delta}
#'       on \eqn{\Delta^0}), \code{cor_S_degree}, \code{cor_S_S0},
#'       \code{gradient_share} (see \code{\link{source_sink_scores}}).}
#'     \item{\code{graphs}}{The input graphs, named by panel, used for layouts.}
#'   }
#'   \code{panel} is \code{""} for a single unnamed graph.  Methods:
#'   \code{print()} summarizes each panel (bandwidth, size, degree diagnostics and
#'   the strongest sources and sinks); \code{plot()} draws it (see
#'   \code{\link{plot.gravity_field}}); \code{as.data.frame()} returns the node,
#'   edge or diagnostics table; \code{c()} combines fields into one multi-panel
#'   field.
#' @seealso \code{\link{plot.gravity_field}}, \code{\link{source_sink_scores}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' library(igraph)
#' A <- matrix(0, 5, 5, dimnames = list(LETTERS[1:5], LETTERS[1:5]))
#' A["A", "B"] <- A["B", "A"] <- 1; A["B", "C"] <- A["C", "B"] <- 1.5
#' A["C", "D"] <- A["D", "C"] <- 0.8; A["D", "E"] <- A["E", "D"] <- 2
#' A["B", "D"] <- A["D", "B"] <- 2.5
#' g <- graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE)
#' f <- gravity_field(g)
#' f
#' head(as.data.frame(f, what = "edges"))
#' plot(f, layout = cbind(x = 1:5, y = c(0, 1, 0, 1, 0)))
#'
#' # One graph at two bandwidths, or several fields combined, as facets
#' f2 <- gravity_field(g, gamma = c(1, 0.5))
#' f3 <- c(local = gravity_field(g, gamma = 1), neutral = gravity_field(g))
#' @export
gravity_field <- function(graph, gamma = 0.5) {
  graphs <- if (igraph::is_igraph(graph)) list(graph) else graph
  if (!is.list(graphs) || !length(graphs) ||
      !all(vapply(graphs, igraph::is_igraph, logical(1))))
    stop("'graph' must be a graph or a list of graphs")
  if (length(graphs) == 1L && length(gamma) > 1L) {
    graphs <- rep(graphs, length(gamma))
    names(graphs) <- sprintf("gamma = %g", gamma)
  }
  pn <- names(graphs)
  if (is.null(pn)) pn <- rep("", length(graphs))
  blank <- !nzchar(pn) | is.na(pn)
  if (length(graphs) > 1L) pn[blank] <- paste("Graph", which(blank))
  else pn[blank] <- ""
  pn <- make.unique(pn, sep = " ")
  gam <- rep_len(gamma, length(graphs))

  parts <- Map(function(g, gm, p) {
    g  <- .gravity_check(g)
    sc <- source_sink_scores(g, gm)
    ed <- gravity_edges(g, gm)
    nodes <- igraph::V(g)$name
    sz <- if ("size" %in% igraph::vertex_attr_names(g)) igraph::V(g)$size else sc$degree
    list(
      graph = g,
      nodes = data.frame(panel = p, Stratum = nodes, degree = sc$degree, size = sz,
                         S = sc$S, gravity = sc$gravity,
                         row.names = NULL, stringsAsFactors = FALSE),
      edges = data.frame(panel = p, from = ed$from, to = ed$to, weight = ed$weight,
                         delta = ed$Delta,
                         source = ifelse(ed$Delta >= 0, ed$from, ed$to),
                         sink   = ifelse(ed$Delta >= 0, ed$to, ed$from),
                         row.names = NULL, stringsAsFactors = FALSE),
      diagnostics = data.frame(panel = p, gamma = gm,
                               degree_share   = attr(sc, "degree_share"),
                               cor_S_degree   = attr(sc, "cor_S_degree"),
                               cor_S_S0       = attr(sc, "cor_S_S0"),
                               gradient_share = attr(sc, "gradient_share"),
                               stringsAsFactors = FALSE))
  }, graphs, gam, pn)

  .new_gravity_field(parts)
}

#' @keywords internal
#' @noRd
.new_gravity_field <- function(parts) {
  stack <- function(what) {
    d <- do.call(rbind, lapply(parts, `[[`, what))
    rownames(d) <- NULL
    d
  }
  out <- list(nodes = stack("nodes"), edges = stack("edges"),
              diagnostics = stack("diagnostics"),
              graphs = stats::setNames(lapply(parts, `[[`, "graph"),
                                       vapply(parts, function(p) p$diagnostics$panel, "")))
  class(out) <- "gravity_field"
  out
}

#' @keywords internal
#' @noRd
.split_gravity_field <- function(x) {
  # Panels are stored in the same order in 'graphs' and 'diagnostics'; index
  # by position, since a single unnamed panel is called "".
  Map(function(p, g)
    list(graph = g,
         nodes = x$nodes[x$nodes$panel == p, , drop = FALSE],
         edges = x$edges[x$edges$panel == p, , drop = FALSE],
         diagnostics = x$diagnostics[x$diagnostics$panel == p, , drop = FALSE]),
    x$diagnostics$panel, unname(x$graphs))
}

#' @rdname gravity_field
#' @param ... For \code{c()}, \code{gravity_field} objects to combine; argument
#'   names become panel names (prefixed to the existing names of multi-panel
#'   fields).  Ignored by \code{print()} and \code{as.data.frame()}.
#' @export
c.gravity_field <- function(...) {
  fields <- list(...)
  if (!all(vapply(fields, inherits, logical(1), what = "gravity_field")))
    stop("Only gravity_field objects can be combined with c()")
  nm <- names(fields)
  if (is.null(nm)) nm <- rep("", length(fields))
  parts <- unlist(Map(function(f, n) {
    ps <- .split_gravity_field(f)
    if (nzchar(n)) {
      lab <- if (length(ps) == 1L) n else paste0(n, ": ", f$diagnostics$panel)
      ps <- Map(function(p, l) {
        p$nodes$panel <- l; p$edges$panel <- l; p$diagnostics$panel <- l
        p
      }, ps, lab)
    }
    ps
  }, fields, nm), recursive = FALSE)
  pn <- vapply(parts, function(p) p$diagnostics$panel, "")
  if (anyDuplicated(pn))
    stop("Panel names must be unique; name the arguments, e.g. c(a = f1, b = f2)")
  .new_gravity_field(unname(parts))
}

#' @rdname gravity_field
#' @param x A \code{gravity_field} object.
#' @param n Number of strongest sources and sinks to list (default 3).
#' @export
print.gravity_field <- function(x, n = 3, ...) {
  top <- function(v) if (nrow(v)) paste(sprintf("%s (%.3f)", utils::head(v$Stratum, n),
                                                  utils::head(v$S, n)), collapse = ", ") else "none"
  for (p in .split_gravity_field(x)) {
    d <- p$diagnostics
    title <- if (nzchar(d$panel)) sprintf("Gravity field [%s]", d$panel) else "Gravity field"
    cat(sprintf("%s: %d populations, %d edges, gamma = %g\n", title,
                nrow(p$nodes), nrow(p$edges), d$gamma))
    cat(sprintf("  degree share %.3f, cor(S, degree) %.3f, gradient share %.3f\n",
                d$degree_share, d$cor_S_degree, d$gradient_share))
    nd <- p$nodes[order(-p$nodes$S), ]
    cat(sprintf("  strongest sources: %s\n", top(nd[nd$S > 0, ])))
    sk <- p$nodes[order(p$nodes$S), ]
    cat(sprintf("  strongest sinks:   %s\n", top(sk[sk$S < 0, ])))
  }
  invisible(x)
}

#' @rdname gravity_field
#' @param row.names,optional Ignored.
#' @param what \code{"nodes"} (default), \code{"edges"} or \code{"diagnostics"}.
#' @export
as.data.frame.gravity_field <- function(x, row.names = NULL, optional = FALSE,
                                        what = c("nodes", "edges", "diagnostics"), ...) {
  what <- match.arg(what)
  x[[what]]
}
