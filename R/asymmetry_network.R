#' Network (rewiring) null for graph asymmetry
#'
#' @description
#' Tests whether the overall amount of directional asymmetry in a Population
#' Graph exceeds that of random graphs with the same size or degree
#' distribution.  The graph is rewired \code{nperm} times (via
#' \code{\link{randomize_graph}}), the observed edge-weight multiset is
#' reassigned to the rewired edges, and the graph-level statistic
#' mean \eqn{|\Delta|} is recomputed to build a null distribution.
#'
#' @details
#' This is the broadest of the asymmetry nulls and is deliberately graph-level:
#' because rewiring destroys the identity of individual edges, a per-edge
#' \emph{p}-value is not well defined (a given edge is absent from most rewired
#' graphs).  The test therefore returns a single row summarising the whole
#' graph.  For inference about specific edges of a graph already accepted as
#' significant, prefer \code{\link{asymmetry_permutation}}.  Usually called
#' through \code{\link{asymmetry_significance}} with \code{mode = "existence"}.
#'
#' @param graph An undirected weighted \code{popgraph}/\code{igraph} object.
#' @param nperm Number of rewired graphs to generate (default 999).
#' @param rewire The randomisation mode passed to \code{\link{randomize_graph}}:
#'   \code{"degree"} (default, preserves the degree distribution) or
#'   \code{"full"}.
#' @param ... Ignored; present for interface consistency.
#'
#' @return An object of class \code{"htest"}, which prints as a standard test
#'   and works with \code{broom::tidy()}: \code{statistic} (observed
#'   \code{mean |Delta|}), \code{parameter} (\code{edges} and the number of
#'   valid rewired graphs, \code{nperm}), the one-sided \code{p.value},
#'   \code{estimate} (the \code{null mean |Delta|}), \code{alternative},
#'   \code{method} and \code{data.name}.  Rewired graphs for which the
#'   asymmetry cannot be computed (e.g. those containing isolated nodes) are
#'   excluded from the null distribution.
#'
#' @seealso \code{\link{asymmetry_significance}}, \code{\link{randomize_graph}},
#'   \code{\link{graph_asymmetries}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#'
#' @importFrom igraph E V ecount vcount
#' @export
#' @examples
#' data(lopho)
#' \donttest{
#'   res <- asymmetry_network(lopho, nperm = 19)
#'   res
#' }
asymmetry_network <- function(graph, nperm = 999,
                              rewire = c("degree", "full"), ...) {

  rewire   <- match.arg(rewire)
  dname    <- deparse1(substitute(graph))
  g_obs    <- graph_asymmetries(graph)
  obs_stat <- mean(abs(igraph::E(g_obs)$delta))
  w        <- igraph::E(graph)$weight

  null <- numeric(nperm)
  for (p in seq_len(nperm)) {
    gr <- tryCatch(randomize_graph(graph, mode = rewire),
                   error = function(e) NULL)
    if (is.null(gr)) { null[p] <- NA_real_; next }

    if (is.null(igraph::V(gr)$name))
      igraph::V(gr)$name <- as.character(seq_len(igraph::vcount(gr)))

    ne <- igraph::ecount(gr)
    igraph::E(gr)$weight <- sample(w, size = ne, replace = (length(w) != ne))
    class(gr) <- c("popgraph", "igraph")

    # Rewiring (especially rewire = "full") can leave isolated nodes, for which
    # the bandwidth is undefined; record those graphs as NA rather than abort.
    null[p] <- tryCatch(mean(abs(igraph::E(graph_asymmetries(gr))$delta)),
                        error = function(e) NA_real_)
  }

  # Add-one (biased-up) permutation p-value; see asymmetry_permutation() and
  # Phipson & Smyth (2010, Stat. Appl. Genet. Mol. Biol. 9:Article39).
  B       <- sum(!is.na(null))
  if (B == 0)
    stop("None of the ", nperm, " rewired graphs yielded a valid asymmetry ",
         "statistic",
         if (rewire == "full") "; try rewire = \"degree\"" else "", ".")
  p_value <- (1 + sum(null >= obs_stat, na.rm = TRUE)) / (1 + B)

  structure(list(
    statistic   = c(`mean |Delta|` = obs_stat),
    parameter   = c(edges = igraph::ecount(graph), nperm = B),
    p.value     = p_value,
    estimate    = c(`null mean |Delta|` = mean(null, na.rm = TRUE)),
    alternative = "greater",
    method      = sprintf("Graph-level asymmetry test (%s rewiring null)",
                          if (rewire == "degree") "degree-preserving" else "full"),
    data.name   = dname),
    class = "htest")
}
