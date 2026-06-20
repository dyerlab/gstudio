#' Significance testing for population graph edge asymmetry
#'
#' @description
#' Unified interface for assessing the statistical significance of the
#' directional asymmetry index \eqn{\Delta_{ij}} produced by
#' \code{\link{graph_asymmetries}}.  Because \eqn{\Delta_{ij}} is a function of
#' the full graph topology, edges are not independent observations and no
#' closed-form null is available; the alternatives below are all resampling
#' procedures, each targeting a different null hypothesis.  As with
#' \code{\link{genetic_structure}} and \code{\link{genetic_distance}}, the
#' particular procedure is selected with \code{mode} and the most robust option
#' for edge-level inference is the default.
#'
#' @details
#' The available modes, from the most local (individual sampling) to the most
#' global (graph configuration), are:
#' \describe{
#'   \item{permutation}{\strong{(default)} Fixed-topology permutation test.
#'     The observed adjacency is held fixed while individual labels are
#'     permuted across strata; edge weights are recomputed on the original edge
#'     set and \eqn{\Delta_{ij}} is recalculated.  Tests whether the
#'     \emph{magnitude} of an edge's asymmetry exceeds what the fixed graph
#'     geometry alone would produce.  Requires \code{data} and \code{groups}.
#'     See \code{\link{asymmetry_permutation}}.}
#'   \item{jackknife}{Stability test.  Individuals are resampled with
#'     replacement within each stratum, the graph is re-estimated, and a
#'     confidence interval for each edge's \eqn{\Delta_{ij}} is formed across
#'     resamples.  Tests whether the direction is robust to the particular
#'     individuals sampled.  Requires \code{data} and \code{groups}.  See
#'     \code{\link{asymmetry_jackknife}}.}
#'   \item{bandwidth}{Bandwidth-alignment permutation.  The node bandwidths
#'     \eqn{b_i} are permuted across nodes while the distances and adjacency are
#'     held fixed.  Tests whether the specific pairing of source- and sink-like
#'     bandwidths on an edge is non-random.  Operates on \code{graph} alone.
#'     See \code{\link{asymmetry_bandwidth}}.}
#'   \item{network}{Network (rewiring) null.  The graph is rewired to random
#'     graphs of the same size or degree distribution and a graph-level summary
#'     (mean \eqn{|\Delta|}) is compared to the observed value.  Tests whether
#'     the whole asymmetry pattern is unusual; returns a single graph-level row.
#'     Operates on \code{graph} alone.  See \code{\link{asymmetry_network}}.}
#' }
#'
#' @param graph An undirected weighted \code{popgraph}/\code{igraph} object with
#'   a numeric \code{weight} edge attribute (the same input accepted by
#'   \code{\link{graph_asymmetries}}).
#' @param data The multivariate genotype matrix originally passed to
#'   \code{\link{popgraph}} (via \code{\link{to_mv}}).  Required for the
#'   \code{"permutation"} and \code{"jackknife"} modes, which recompute the
#'   graph from individuals; ignored otherwise.
#' @param groups A factor of stratum membership, one entry per row of
#'   \code{data}.  Required for the \code{"permutation"} and \code{"jackknife"}
#'   modes; ignored otherwise.
#' @param mode The resampling procedure to use.  One of \code{"permutation"}
#'   (default), \code{"jackknife"}, \code{"bandwidth"}, or \code{"network"}.
#' @param nperm Number of permutations / resamples (default 999).
#' @param pendants How to treat \emph{pendant} (leaf) edges — edges incident to
#'   a degree-one node.  On such an edge the directional weight out of the leaf
#'   is forced to \eqn{1} (the leaf has a single neighbour), so the edge's
#'   asymmetry magnitude is fixed by topology and its permutation \emph{p}-value
#'   is \strong{anti-conservatively biased toward significance}.  One of
#'   \code{"warn"} (default: keep the edges but emit a warning naming how many
#'   are pendant), \code{"keep"} (keep them silently), or \code{"drop"} (remove
#'   pendant edges from the result; the count removed is recorded in
#'   \code{attr(x, "pendants_dropped")}).  Ignored for \code{mode = "network"},
#'   which returns a single graph-level row.  This acts at the edge level: it
#'   does not re-estimate the bandwidths of interior nodes that neighbour a
#'   leaf; to remove leaf influence entirely, prune the degree-one nodes from
#'   \code{graph} before calling.
#' @param ... Additional arguments passed to the dispatched base function
#'   (e.g. \code{conf} for \code{"jackknife"}, \code{mode} for
#'   \code{"network"}, or \code{alpha} for the internal \code{popgraph} calls).
#'
#' @return A \code{data.frame} with one row per retained edge (one row total for
#'   \code{mode = "network"}) and the columns:
#'   \describe{
#'     \item{from, to}{Endpoint names of the edge (\code{NA} for the
#'       graph-level \code{"network"} summary).}
#'     \item{delta}{The observed asymmetry index \eqn{\Delta_{ij}} (or observed
#'       mean \eqn{|\Delta|} for \code{"network"}).}
#'     \item{statistic}{The test statistic actually compared to the null
#'       distribution.}
#'     \item{p_value}{Two-tailed permutation \emph{p}-value (\code{NA} for
#'       \code{"jackknife"}).}
#'     \item{ci_low, ci_high}{Resampling confidence bounds (\code{NA} except for
#'       \code{"jackknife"}).}
#'   }
#'
#' @seealso \code{\link{graph_asymmetries}} for the asymmetry computation;
#'   \code{\link{asymmetry_permutation}}, \code{\link{asymmetry_jackknife}},
#'   \code{\link{asymmetry_bandwidth}}, \code{\link{asymmetry_network}} for the
#'   individual procedures.
#'
#' @references
#' Dyer RJ, Nason JD (2004) Population Graphs: the graph theoretic shape of
#' genetic structure. \emph{Evolution} \strong{58}: 1605--1615.
#'
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#'
#' @examples
#' \donttest{
#' data(arapat)
#' mv     <- to_mv(arapat)
#' groups <- arapat$Population
#' graph  <- popgraph(mv, groups)
#'
#' # Fixed-topology permutation test (default)
#' asymmetry_significance(graph, data = mv, groups = groups, nperm = 99)
#'
#' # Stability (jackknife) confidence intervals
#' asymmetry_significance(graph, data = mv, groups = groups,
#'                        mode = "jackknife", nperm = 99)
#'
#' # Bandwidth-alignment null, no individual data required
#' asymmetry_significance(graph, mode = "bandwidth", nperm = 199)
#' }
#'
#' @importFrom igraph is_igraph is_directed E
#' @export
asymmetry_significance <- function(graph,
                                   data   = NULL,
                                   groups = NULL,
                                   mode   = c("permutation", "jackknife",
                                              "bandwidth", "network"),
                                   nperm  = 999,
                                   pendants = c("warn", "keep", "drop"),
                                   ...) {

  mode     <- match.arg(mode)
  pendants <- match.arg(pendants)

  # ---- graph validation (shared with graph_asymmetries) -------------------
  if (!igraph::is_igraph(graph))
    stop("'graph' must be an igraph/popgraph object")
  if (igraph::is_directed(graph))
    stop("'graph' must be undirected")
  if (is.null(igraph::E(graph)$weight))
    stop("'graph' must have a numeric 'weight' edge attribute")

  # ---- individual-data validation for the data-driven modes ---------------
  needs_data <- mode %in% c("permutation", "jackknife")
  if (needs_data) {
    if (is.null(data) || is.null(groups))
      stop(sprintf("mode = '%s' requires both 'data' and 'groups'.", mode))
    if (!is.matrix(data))
      stop("'data' must be a numeric matrix (use to_mv() on your genotypes).")
    if (length(groups) != nrow(data))
      stop("'groups' must have one entry per row of 'data'.")
  }

  # ---- dispatch -----------------------------------------------------------
  ret <- switch(
    mode,
    permutation = asymmetry_permutation(graph, data, groups, nperm = nperm, ...),
    jackknife   = asymmetry_jackknife(graph, data, groups, nperm = nperm, ...),
    bandwidth   = asymmetry_bandwidth(graph, nperm = nperm, ...),
    network     = asymmetry_network(graph, nperm = nperm, ...)
  )

  # ---- pendant (leaf) edge handling ---------------------------------------
  # Edges incident to a degree-one node carry a topologically forced direction
  # (the leaf's only outgoing weight is 1), inflating |Delta| and biasing the
  # permutation p-value toward significance. Warn about or drop them on request.
  if (mode != "network" && pendants != "keep" &&
      !is.null(ret$from) && any(!is.na(ret$from))) {
    deg       <- igraph::degree(graph)
    leaves    <- names(deg)[deg <= 1L]
    is_pend   <- ret$from %in% leaves | ret$to %in% leaves
    n_pend    <- sum(is_pend, na.rm = TRUE)
    if (n_pend > 0L) {
      if (pendants == "warn") {
        warning(sprintf(paste0(
          "%d of %d edges are incident to a degree-one (pendant/leaf) node. ",
          "The direction on a pendant edge is topologically forced, so its ",
          "asymmetry magnitude is inflated and its p-value is anti-conservatively ",
          "biased toward significance. Interpret these edges with caution, or call ",
          "with pendants = 'drop' to exclude them."),
          n_pend, nrow(ret)))
      } else {                                   # pendants == "drop"
        ret <- ret[!is_pend, , drop = FALSE]
        rownames(ret) <- NULL
        attr(ret, "pendants_dropped") <- n_pend
      }
    }
  }

  attr(ret, "mode") <- mode
  return(ret)
}
