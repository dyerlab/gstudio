#' Bandwidth-alignment permutation test for edge asymmetry
#'
#' @description
#' Tests whether the asymmetry on each edge arises from the specific alignment
#' of node bandwidths rather than from the global distribution of bandwidths.
#' The node bandwidths \eqn{b_i} are permuted across nodes while the edge
#' weights (distances) and the adjacency are held fixed, and \eqn{\Delta_{ij}}
#' is recomputed for each permutation.  This is the narrowest of the asymmetry
#' nulls: it randomizes only \emph{which} node carries \emph{which} "genetic
#' gravity," isolating the bandwidth assignment from both distance and topology.
#'
#' @details
#' Operates on the graph alone (no individual genotype data are required).
#' Usually called through \code{\link{asymmetry_significance}} with
#' \code{mode = "bandwidth"}.
#'
#' @param graph An undirected weighted \code{popgraph}/\code{igraph} object.
#' @param nperm Number of bandwidth permutations (default 999).
#' @param ... Ignored; present for interface consistency.
#'
#' @return A \code{data.frame} with columns \code{from}, \code{to},
#'   \code{delta}, \code{statistic}, \code{p_value}, \code{ci_low},
#'   \code{ci_high} (the last two \code{NA} for this mode).
#'
#' @seealso \code{\link{asymmetry_significance}}, \code{\link{graph_asymmetries}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#'
#' @importFrom igraph as_edgelist neighbors get_edge_ids E V
#' @export
asymmetry_bandwidth <- function(graph, nperm = 999, ...) {

  g_obs     <- graph_asymmetries(graph)
  delta_obs <- igraph::E(g_obs)$delta
  b_obs     <- igraph::V(g_obs)$bandwidth
  el        <- igraph::as_edgelist(g_obs, names = TRUE)

  null <- replicate(nperm, .delta_with_bandwidth(graph, sample(b_obs)))
  # replicate() returns a matrix (ne x nperm) when ne > 1; coerce for safety.
  null <- matrix(null, nrow = nrow(el))

  # Add-one (biased-up) permutation p-value; see asymmetry_permutation() and
  # Phipson & Smyth (2010, Stat. Appl. Genet. Mol. Biol. 9:Article39).
  n_ge    <- rowSums(abs(null) >= abs(delta_obs), na.rm = TRUE)
  B       <- rowSums(!is.na(null))
  p_value <- (1 + n_ge) / (1 + B)

  data.frame(
    from      = el[, 1],
    to        = el[, 2],
    delta     = delta_obs,
    statistic = delta_obs,
    p_value   = p_value,
    ci_low    = NA_real_,
    ci_high   = NA_real_,
    stringsAsFactors = FALSE
  )
}


# Internal: recompute the per-edge asymmetry index for an arbitrary bandwidth
# vector 'b' (assumed aligned to V(graph) order).  Mirrors the kernel
# normalisation in graph_asymmetries() but takes the bandwidths as given.
#' @keywords internal
#' @noRd
.delta_with_bandwidth <- function(graph, b) {
  nodes <- igraph::V(graph)$name
  names(b) <- nodes

  kern <- lapply(nodes, function(u) {
    nbs <- igraph::neighbors(graph, u, mode = "all")$name
    d <- vapply(nbs, function(n) {
      igraph::E(graph)[igraph::get_edge_ids(graph, c(u, n),
                                            directed = FALSE)]$weight
    }, numeric(1))
    k <- exp(-d^2 / (2 * b[u]^2))
    k / sum(k)
  })
  names(kern) <- nodes

  el     <- igraph::as_edgelist(graph, names = TRUE)
  w_away <- mapply(function(u, v) kern[[u]][v], el[, 1], el[, 2])
  w_to   <- mapply(function(u, v) kern[[v]][u], el[, 1], el[, 2])

  unname(w_away - w_to)
}
