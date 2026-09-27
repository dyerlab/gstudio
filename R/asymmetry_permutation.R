#' Fixed-topology permutation test for edge asymmetry
#'
#' @description
#' Tests whether the magnitude of each edge's asymmetry index
#' \eqn{\Delta_{ij}} exceeds what is expected given the fixed geometry of the
#' Population Graph.  The observed adjacency is held fixed; individual labels
#' are permuted across strata, the conditional edge weights are recomputed on
#' the \emph{original} edge set, and \eqn{\Delta_{ij}} is recalculated.  A
#' two-tailed \emph{p}-value per edge is the fraction of permutations whose
#' \eqn{|\Delta_{\mathrm{null}}|} meets or exceeds the observed
#' \eqn{|\Delta_{\mathrm{obs}}|}.
#'
#' @details
#' Because the edge set never changes across permutations, every retained edge
#' receives a full, equally sized null distribution (avoiding the "denominator
#' problem" of re-pruning the graph), and the purely topological component of
#' \eqn{\Delta_{ij}} (for example the inflated values incident to low-degree
#' nodes) is reproduced in the null rather than mistaken for signal.
#'
#' Fixed-topology edge weights are obtained by re-running \code{\link{popgraph}}
#' on the permuted labels with \code{alpha = 1}, which retains all node pairs so
#' that the returned weighted adjacency is the full conditional-distance matrix;
#' the observed edges then read their permuted weights from that matrix.  This is
#' usually called through \code{\link{asymmetry_significance}} with
#' \code{mode = "location"}.
#'
#' @param graph An undirected weighted \code{popgraph}/\code{igraph} object.
#' @param data The multivariate genotype matrix passed to \code{\link{popgraph}}.
#' @param groups A factor of stratum membership, one entry per row of \code{data}.
#' @param nperm Number of label permutations (default 999).
#' @param ... Additional arguments passed to \code{\link{popgraph}} (e.g.
#'   \code{tol}).  \code{alpha} is ignored with a warning, since the test
#'   requires \code{alpha = 1}.
#'
#' @return A \code{data.frame} with columns \code{from}, \code{to},
#'   \code{delta}, \code{statistic}, and \code{p_value}.
#'
#' @seealso \code{\link{asymmetry_significance}}, \code{\link{graph_asymmetries}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#'
#' @importFrom igraph as_edgelist E
#' @export
asymmetry_permutation <- function(graph, data, groups, nperm = 999, ...) {

  .validate_asymmetry_groups(graph, groups)
  groups    <- factor(as.character(groups))
  g_obs     <- graph_asymmetries(graph)
  el        <- igraph::as_edgelist(g_obs, names = TRUE)
  delta_obs <- igraph::E(g_obs)$delta
  ne        <- nrow(el)

  dots <- list(...)
  if (!is.null(dots$alpha)) {
    warning("'alpha' is ignored: the fixed-topology test always uses alpha = 1 ",
            "so that every observed edge receives a permuted weight.")
    dots$alpha <- NULL
  }

  null <- matrix(NA_real_, nrow = ne, ncol = nperm)

  for (p in seq_len(nperm)) {
    perm_groups <- sample(groups)

    # alpha = 1 retains every pair, so the weighted adjacency is the full
    # conditional-distance matrix under the permuted labels.  A permutation
    # that fails leaves its column NA and is dropped from B below.
    delta_p <- tryCatch({
      g_full <- suppressWarnings(do.call(popgraph,
                                         c(list(data, perm_groups, alpha = 1), dots)))
      Wp     <- to_matrix(g_full, mode = "edge weight")

      g_null <- graph
      # Index the permuted distance matrix by the OBSERVED endpoint names so
      # the adjacency of 'graph' is preserved exactly.
      igraph::E(g_null)$weight <- Wp[el]

      igraph::E(graph_asymmetries(g_null))$delta
    }, error = function(e) NULL)

    if (!is.null(delta_p)) null[, p] <- delta_p
  }

  # Add-one (biased-up) permutation p-value: (1 + #{|null| >= |obs|}) / (1 + B).
  # Including the observed configuration in the reference set keeps the estimator
  # valid and bounded away from zero, avoiding the anti-conservative bias of the
  # raw fraction (Phipson & Smyth 2010, Stat. Appl. Genet. Mol. Biol. 9:Article39).
  n_ge    <- rowSums(abs(null) >= abs(delta_obs), na.rm = TRUE)
  B       <- rowSums(!is.na(null))
  p_value <- (1 + n_ge) / (1 + B)

  data.frame(
    from      = el[, 1],
    to        = el[, 2],
    delta     = delta_obs,
    statistic = delta_obs,
    p_value   = p_value,
    stringsAsFactors = FALSE
  )
}
