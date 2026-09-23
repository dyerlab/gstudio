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
#' particular procedure is selected with \code{mode}; the default is the
#' omnibus, graph-wide test, since it requires only \code{graph} and answers
#' the question that should be asked before any of the edge-level modes below
#' are worth running.
#'
#' @details
#' The four modes form a hierarchy of interest, from an omnibus test of the
#' whole graph down to the evidentiary support for a single edge, and each
#' level's question is narrower than -- and conditional on -- the one above it:
#' \describe{
#'   \item{existence}{\strong{(default) Is there any asymmetry?}  \eqn{H_0}:
#'     \eqn{\Delta_{ij} = 0} for every edge in the graph -- is there any
#'     asymmetry anywhere?  The graph is rewired to random graphs of the same
#'     size or degree distribution and the observed graph-level mean
#'     \eqn{|\Delta|} is compared to that null distribution.  Failing to
#'     reject leaves no basis for treating any single edge as asymmetric, and
#'     no reason to proceed to the edge-level modes below.  Operates on
#'     \code{graph} alone.  See \code{\link{asymmetry_network}}.}
#'   \item{location}{\strong{Which edge(s)?}  Given that asymmetry may exist
#'     somewhere in the graph, \eqn{H_0} for a given edge is that its
#'     \eqn{\Delta_{ij}} is no larger than fixed-topology sampling noise would
#'     produce; the observed adjacency is held fixed while individual labels
#'     are permuted across strata, edge weights are recomputed on the original
#'     edge set, and \eqn{\Delta_{ij}} is recalculated.  Failing to reject
#'     means this edge does not stand out as asymmetric, whatever the
#'     existence result.  Requires \code{data} and \code{groups}.  See
#'     \code{\link{asymmetry_permutation}}.}
#'   \item{mechanism}{\strong{Why that edge?}  Given an edge that does stand
#'     out, \eqn{H_0} is that the specific pairing of source- and sink-like
#'     bandwidths on it is exchangeable with any other pairing; node
#'     bandwidths \eqn{b_i} are permuted across nodes while the distances and
#'     adjacency are held fixed.  Failing to reject means the edge's direction
#'     is not explained by its endpoints' relative bandwidths -- if the
#'     asymmetry is real, something other than node size is producing it.
#'     Operates on \code{graph} alone.  See \code{\link{asymmetry_bandwidth}}.}
#'   \item{support}{\strong{How much to trust it?}  For an edge you are
#'     otherwise prepared to call asymmetric, the question is whether that
#'     estimate itself has support: is \eqn{\Delta_{ij}}'s direction stable, or
#'     an artifact of which individuals happened to be sampled?  Individuals
#'     are resampled with replacement within each stratum, the graph is
#'     re-estimated, and a confidence interval for \eqn{\Delta_{ij}} is formed
#'     across resamples.  A confidence interval straddling zero means the
#'     direction lacks the evidentiary support to be reported on its own,
#'     however it fared on the modes above.  Requires \code{data} and
#'     \code{groups}.  See \code{\link{asymmetry_jackknife}}.}
#' }
#'
#' @param graph An undirected weighted \code{popgraph}/\code{igraph} object with
#'   a numeric \code{weight} edge attribute (the same input accepted by
#'   \code{\link{graph_asymmetries}}).
#' @param data The multivariate genotype matrix originally passed to
#'   \code{\link{popgraph}} (via \code{\link{to_mv}}).  Required for the
#'   \code{"location"} and \code{"support"} modes, which recompute the
#'   graph from individuals; ignored otherwise.
#' @param groups A factor of stratum membership, one entry per row of
#'   \code{data}.  Required for the \code{"location"} and \code{"support"}
#'   modes; ignored otherwise.
#' @param mode The resampling procedure to use.  One of \code{"existence"}
#'   (default), \code{"location"}, \code{"mechanism"}, or \code{"support"}.
#' @param nperm Number of permutations / resamples (default 999).
#' @param pendants How to treat \emph{pendant} (leaf) edges — edges incident to
#'   a degree-one node.  On such an edge the directional weight out of the leaf
#'   is forced to \eqn{1} (the leaf has a single neighbour), so the edge's
#'   asymmetry magnitude is fixed by topology and its permutation \emph{p}-value
#'   is \strong{anti-conservatively biased toward significance}.  One of
#'   \code{"warn"} (default: keep the edges but emit a warning naming how many
#'   are pendant), \code{"keep"} (keep them silently), or \code{"drop"} (remove
#'   pendant edges from the result; the count removed is recorded in
#'   \code{attr(x, "pendants_dropped")}).  Ignored for \code{mode = "existence"},
#'   which returns a single graph-level row.  This acts at the edge level: it
#'   does not re-estimate the bandwidths of interior nodes that neighbour a
#'   leaf; to remove leaf influence entirely, prune the degree-one nodes from
#'   \code{graph} before calling.
#' @param ... Additional arguments passed to the dispatched base function
#'   (e.g. \code{conf} for \code{"support"}, \code{rewire} for
#'   \code{"existence"}, or \code{alpha} for the internal \code{popgraph} calls).
#'
#' @return A \code{data.frame} with one row per retained edge (one row total for
#'   \code{mode = "existence"}) and the columns:
#'   \describe{
#'     \item{from, to}{Endpoint names of the edge (\code{NA} for the
#'       graph-level \code{"existence"} summary).}
#'     \item{delta}{The observed asymmetry index \eqn{\Delta_{ij}} (or observed
#'       mean \eqn{|\Delta|} for \code{"existence"}).}
#'     \item{statistic}{The test statistic actually compared to the null
#'       distribution.}
#'     \item{p_value}{Two-tailed permutation \emph{p}-value (\code{NA} for
#'       \code{"support"}), computed with the add-one correction
#'       \eqn{(1 + \#\{|\Delta_{\mathrm{null}}| \ge |\Delta_{\mathrm{obs}}|\}) /
#'       (1 + B)} over \eqn{B} permutations, so it is strictly positive
#'       (Phipson & Smyth 2010).}
#'     \item{ci_low, ci_high}{Resampling confidence bounds (\code{NA} except for
#'       \code{"support"}).}
#'   }
#'
#' @seealso \code{\link{graph_asymmetries}} for the asymmetry computation;
#'   \code{\link{asymmetry_network}} (\code{"existence"}),
#'   \code{\link{asymmetry_permutation}} (\code{"location"}),
#'   \code{\link{asymmetry_bandwidth}} (\code{"mechanism"}),
#'   \code{\link{asymmetry_jackknife}} (\code{"support"}) for the individual
#'   procedures.
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
#' # Existence: is there any asymmetry in the graph at all? (default)
#' asymmetry_significance(graph, nperm = 199)
#'
#' # Location: which edge(s) are asymmetric?
#' asymmetry_significance(graph, data = mv, groups = groups,
#'                        mode = "location", nperm = 99)
#'
#' # Mechanism: is a specific node bandwidth pairing driving an edge?
#' asymmetry_significance(graph, mode = "mechanism", nperm = 199)
#'
#' # Support: how much confidence do we have in an edge's direction?
#' asymmetry_significance(graph, data = mv, groups = groups,
#'                        mode = "support", nperm = 99)
#' }
#'
#' @importFrom igraph is_igraph is_directed E
#' @export
asymmetry_significance <- function(graph,
                                   data   = NULL,
                                   groups = NULL,
                                   mode   = c("existence", "location",
                                              "mechanism", "support"),
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
  needs_data <- mode %in% c("location", "support")
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
    location  = asymmetry_permutation(graph, data, groups, nperm = nperm, ...),
    support   = asymmetry_jackknife(graph, data, groups, nperm = nperm, ...),
    mechanism = asymmetry_bandwidth(graph, nperm = nperm, ...),
    existence = asymmetry_network(graph, nperm = nperm, ...)
  )

  # ---- pendant (leaf) edge handling ---------------------------------------
  # Edges incident to a degree-one node carry a topologically forced direction
  # (the leaf's only outgoing weight is 1), inflating |Delta| and biasing the
  # permutation p-value toward significance. Warn about or drop them on request.
  if (mode != "existence" && pendants != "keep" &&
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
