#' Significance testing for population graph edge asymmetry
#'
#' @description
#' Unified interface for assessing the statistical significance of the
#' directional asymmetry index \eqn{\Delta_{ij}} produced by
#' \code{\link{graph_asymmetries}}.  The asymmetry index is derived from the
#' conditional genetic distances among strata, the topology of the Population
#' Graph, and the node bandwidths estimated from them, none of which has a
#' closed-form sampling distribution; inference must therefore rely on
#' resampling.  As with \code{\link{genetic_structure}} and
#' \code{\link{genetic_distance}}, the particular test is selected with
#' \code{mode}; the default is the omnibus, graph-wide test, since it requires
#' only \code{graph} and answers the question that should be asked before any
#' of the edge-level modes are worth running.
#'
#' @details
#' Significance is assessed by permutation through a hierarchy of successively
#' conditioned hypotheses: whether the graph shows any directional structure at
#' all (\code{"existence"}), which edges carry it (\code{"location"}), and
#' whether an edge's asymmetry reflects properties of the populations it
#' connects (\code{"mechanism"}).  Each question is only worth asking once the
#' one before it has been rejected.  For edges that survive this hierarchy,
#' \code{\link{asymmetry_ci}} provides a bootstrap confidence interval on
#' \eqn{\Delta_{ij}} for reporting its magnitude.
#' \describe{
#'   \item{existence}{\strong{(default) Is there any asymmetry?}  This
#'     hypothesis tests whether gene flow across the whole system has
#'     directional structure, such as source--sink dynamics, range expansion,
#'     or movement along currents, winds or downstream corridors, beyond what
#'     any network of populations with the same size and connectivity would
#'     show.  The graph is rewired to random graphs of the same size or degree
#'     distribution and the observed graph-level mean \eqn{|\Delta|} is
#'     compared to that null distribution.  Failing to reject this null
#'     indicates that the overall asymmetry in the graph is no greater than a
#'     randomly connected set of populations would produce, so there is no
#'     evidence that gene flow in this system has a preferred direction, and no
#'     basis for asking which edges carry it.  Operates on \code{graph} alone.
#'     See \code{\link{asymmetry_network}}.}
#'   \item{location}{\strong{Which edge(s)?}  This hypothesis tests whether a
#'     particular pair of connected populations exchanges genes in one
#'     direction more than the other, marking that connection as a specific
#'     corridor of directional gene flow rather than just part of the general
#'     pattern of connectivity.  The observed adjacency is held fixed while
#'     individual labels are permuted across strata, edge weights are
#'     recomputed on the original edge set, and \eqn{\Delta_{ij}} is
#'     recalculated.  Failing to reject this null indicates that the apparent
#'     directionality between these two populations is no stronger than would
#'     be seen if individuals were assigned to populations at random, so this
#'     connection cannot be singled out as a directional corridor, even if gene
#'     flow across the system as a whole has a preferred direction.  Requires
#'     \code{data} and \code{groups}.  See \code{\link{asymmetry_permutation}}.}
#'   \item{mechanism}{\strong{Why that edge?}  This hypothesis tests whether
#'     the apparent direction of gene flow along an edge comes from differences
#'     between the two populations themselves (how genetically close or
#'     isolated each is from its own neighbours, the kind of difference that
#'     separates a source from a sink) rather than from where they sit in the
#'     wider network of connectivity.  Node bandwidths \eqn{b_i} are permuted
#'     across nodes while the distances and adjacency are held fixed.  Failing
#'     to reject this null indicates that the edge's asymmetry comes from the
#'     fixed structure of the graph (its topology, node degrees and edge
#'     distances), not from which bandwidth each endpoint carries.  Operates on
#'     \code{graph} alone.  See \code{\link{asymmetry_bandwidth}}.}
#' }
#'
#' @param graph An undirected weighted \code{popgraph}/\code{igraph} object with
#'   a numeric \code{weight} edge attribute (the same input accepted by
#'   \code{\link{graph_asymmetries}}).
#' @param data The multivariate genotype matrix originally passed to
#'   \code{\link{popgraph}} (via \code{\link{to_mv}}).  Required for
#'   \code{mode = "location"}, which recomputes edge weights from individuals;
#'   ignored otherwise.
#' @param groups A factor of stratum membership, one entry per row of
#'   \code{data}.  Required for \code{mode = "location"}; ignored otherwise.
#' @param mode The permutation test to use.  One of \code{"existence"}
#'   (default), \code{"location"}, or \code{"mechanism"}.
#' @param nperm Number of permutations (default 999).
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
#'   (e.g. \code{rewire} for \code{"existence"}, or \code{alpha} for the
#'   internal \code{popgraph} calls).
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
#'     \item{p_value}{Two-tailed permutation \emph{p}-value, computed with the
#'       add-one correction
#'       \eqn{(1 + \#\{|\Delta_{\mathrm{null}}| \ge |\Delta_{\mathrm{obs}}|\}) /
#'       (1 + B)} over \eqn{B} permutations, so it is strictly positive
#'       (Phipson & Smyth 2010).}
#'   }
#'
#' @seealso \code{\link{graph_asymmetries}} for the asymmetry computation;
#'   \code{\link{asymmetry_network}} (\code{"existence"}),
#'   \code{\link{asymmetry_permutation}} (\code{"location"}), and
#'   \code{\link{asymmetry_bandwidth}} (\code{"mechanism"}) for the individual
#'   tests; \code{\link{asymmetry_ci}} for confidence intervals on
#'   \eqn{\Delta_{ij}}.
#'
#' @references
#' Dyer RJ, Nason JD (2004) Population Graphs: the graph theoretic shape of
#' genetic structure. \emph{Evolution} \strong{58}: 1605--1615.
#'
#' Phipson B, Smyth GK (2010) Permutation p-values should never be zero:
#' calculating exact p-values when permutations are randomly drawn.
#' \emph{Statistical Applications in Genetics and Molecular Biology}
#' \strong{9}: Article 39.
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
#' # Existence: is there any directional structure in the graph? (default)
#' asymmetry_significance(graph, nperm = 199)
#'
#' # Location: which edge(s) carry it?
#' asymmetry_significance(graph, data = mv, groups = groups,
#'                        mode = "location", nperm = 99)
#'
#' # Mechanism: does an edge's asymmetry reflect its populations' bandwidths?
#' asymmetry_significance(graph, mode = "mechanism", nperm = 199)
#'
#' # Then report the magnitude of a retained edge with a confidence interval
#' asymmetry_ci(graph, data = mv, groups = groups, nboot = 99)
#' }
#'
#' @export
asymmetry_significance <- function(graph,
                                   data   = NULL,
                                   groups = NULL,
                                   mode   = c("existence", "location",
                                              "mechanism"),
                                   nperm  = 999,
                                   pendants = c("warn", "keep", "drop"),
                                   ...) {

  mode     <- match.arg(mode)
  pendants <- match.arg(pendants)

  .validate_asymmetry_graph(graph)
  if (mode == "location") {
    if (is.null(data) || is.null(groups))
      stop("mode = 'location' requires both 'data' and 'groups'.")
    .validate_asymmetry_data(data, groups)
  }

  ret <- switch(
    mode,
    existence = asymmetry_network(graph, nperm = nperm, ...),
    location  = asymmetry_permutation(graph, data, groups, nperm = nperm, ...),
    mechanism = asymmetry_bandwidth(graph, nperm = nperm, ...)
  )

  if (mode != "existence")
    ret <- .handle_pendant_edges(ret, graph, pendants)

  attr(ret, "mode") <- mode
  return(ret)
}


# Internal: shared input validation for the asymmetry inference functions.
#' @importFrom igraph is_igraph is_directed E
#' @keywords internal
#' @noRd
.validate_asymmetry_graph <- function(graph) {
  if (!igraph::is_igraph(graph))
    stop("'graph' must be an igraph/popgraph object")
  if (igraph::is_directed(graph))
    stop("'graph' must be undirected")
  if (is.null(igraph::E(graph)$weight))
    stop("'graph' must have a numeric 'weight' edge attribute")
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.validate_asymmetry_data <- function(data, groups) {
  if (is.null(data) || is.null(groups))
    stop("both 'data' and 'groups' are required.")
  if (!is.matrix(data))
    stop("'data' must be a numeric matrix (use to_mv() on your genotypes).")
  if (length(groups) != nrow(data))
    stop("'groups' must have one entry per row of 'data'.")
  invisible(TRUE)
}


# Internal: pendant (leaf) edge handling for per-edge results.
# Edges incident to a degree-one node carry a topologically forced direction
# (the leaf's only outgoing weight is 1), inflating |Delta| and biasing
# permutation p-values toward significance. Warn about or drop them on request.
#' @importFrom igraph degree
#' @keywords internal
#' @noRd
.handle_pendant_edges <- function(ret, graph, pendants) {
  if (pendants == "keep" || is.null(ret$from) || all(is.na(ret$from)))
    return(ret)

  deg     <- igraph::degree(graph)
  leaves  <- names(deg)[deg <= 1L]
  is_pend <- ret$from %in% leaves | ret$to %in% leaves
  n_pend  <- sum(is_pend, na.rm = TRUE)
  if (n_pend == 0L)
    return(ret)

  if (pendants == "warn") {
    warning(sprintf(paste0(
      "%d of %d edges are incident to a degree-one (pendant/leaf) node. ",
      "The direction on a pendant edge is topologically forced, so its ",
      "asymmetry magnitude is inflated and its p-value is anti-conservatively ",
      "biased toward significance. Interpret these edges with caution, or call ",
      "with pendants = 'drop' to exclude them."),
      n_pend, nrow(ret)))
  } else {                                        # pendants == "drop"
    ret <- ret[!is_pend, , drop = FALSE]
    rownames(ret) <- NULL
    attr(ret, "pendants_dropped") <- n_pend
  }
  ret
}
