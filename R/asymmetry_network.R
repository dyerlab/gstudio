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
#' through \code{\link{asymmetry_significance}} with \code{mode = "network"}.
#'
#' @param graph An undirected weighted \code{popgraph}/\code{igraph} object.
#' @param nperm Number of rewired graphs to generate (default 999).
#' @param rewire The randomisation mode passed to \code{\link{randomize_graph}}:
#'   \code{"degree"} (default, preserves the degree distribution) or
#'   \code{"full"}.
#' @param ... Ignored; present for interface consistency.
#'
#' @return A one-row \code{data.frame} with columns \code{from} and \code{to}
#'   equal to \code{NA}, \code{delta} and \code{statistic} equal to the observed
#'   mean \eqn{|\Delta|}, the \code{p_value}, and \code{ci_low}/\code{ci_high}
#'   equal to \code{NA}.
#'
#' @seealso \code{\link{asymmetry_significance}}, \code{\link{randomize_graph}},
#'   \code{\link{graph_asymmetries}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#'
#' @importFrom igraph E V ecount vcount
#' @export
asymmetry_network <- function(graph, nperm = 999,
                              rewire = c("degree", "full"), ...) {

  rewire   <- match.arg(rewire)
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
    class(gr) <- c("igraph", "popgraph")

    null[p] <- mean(abs(igraph::E(graph_asymmetries(gr))$delta))
  }

  # Add-one (biased-up) permutation p-value; see asymmetry_permutation() and
  # Phipson & Smyth (2010, Stat. Appl. Genet. Mol. Biol. 9:Article39).
  B       <- sum(!is.na(null))
  p_value <- (1 + sum(null >= obs_stat, na.rm = TRUE)) / (1 + B)

  data.frame(
    from      = NA_character_,
    to        = NA_character_,
    delta     = obs_stat,
    statistic = obs_stat,
    p_value   = p_value,
    ci_low    = NA_real_,
    ci_high   = NA_real_,
    stringsAsFactors = FALSE
  )
}
