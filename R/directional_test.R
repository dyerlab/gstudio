#' Nonparametric test for directional bias along an a priori axis
#'
#' @description
#' Tests whether the directional arc weights of a Population Graph are biased
#' along a pre-specified spatial or ordinal axis (e.g. downstream along a river,
#' down an elevation gradient, or by stepping-stone position).  For each edge the
#' pairwise-normalised arc weight
#' \eqn{\tilde{w}_{i \to j} = w_{i \to j} / (w_{i \to j} + w_{j \to i})} is the
#' share of the edge's directional weight pointing in the \dQuote{forward}
#' direction (from the lower- to the higher-\code{orientation} endpoint).  Under
#' symmetric connectivity \eqn{\tilde{w}} sits at a per-edge center; a one-sample
#' nonparametric test of the signed departures over all edges asks whether the
#' graph carries net flow along the axis.
#'
#' @details
#' This is the design-driven (\dQuote{Level 2}) member of the asymmetry-testing
#' hierarchy: a single, pre-registered, typically one-tailed contrast aligned
#' with an external hypothesis.  It is far more powerful than scanning every edge
#' or node, and—because it collapses the graph to one directional statistic—it
#' sidesteps the non-independence of per-edge tests.
#'
#' \strong{The center is not 0.5.}  In the no-direction limit (all of a node's
#' neighbour distances equal), the softmax weights reduce to \eqn{1/k}, so the
#' forward arc weight equals \eqn{k_{\mathrm{to}} / (k_{\mathrm{from}} +
#' k_{\mathrm{to}})}, the \emph{degree-expected} center.  This equals 0.5 only
#' when the two endpoints have equal degree; otherwise symmetric flow still
#' pushes \eqn{\tilde{w}} toward the higher-degree node.  Testing against 0.5
#' (\code{center = "half"}) therefore inherits an anti-conservative,
#' degree-driven bias; the default \code{center = "degree"} removes it
#' analytically.  (The exact symmetric-null center for a structured neighborhood
#' is whatever an isotropic/permutation reference gives; the degree center is the
#' leading-order analytic approximation.)
#'
#' \strong{Non-independence caveat.}  Edges that share a node share inputs, so the
#' effective sample size is below the edge count and the nominal variance of the
#' signed-rank or sign test is mildly optimistic.  Treat borderline p-values with
#' care; a node-clustered or permutation-calibrated variant is the conservative
#' alternative.
#'
#' @param graph An undirected \code{popgraph}/\code{igraph} with a numeric
#'   \code{weight} edge attribute.  If the directional attributes
#'   (\code{w_away}, \code{w_to}) are absent they are computed via
#'   \code{\link{graph_asymmetries}}.
#' @param orientation Numeric vector giving each node's position on the a priori
#'   axis (larger = \dQuote{downstream}/forward).  Either named by node
#'   (matched to \code{V(graph)$name}) or unnamed in vertex order.  Edges whose
#'   endpoints tie on \code{orientation} cannot be oriented and are dropped.
#' @param center Per-edge symmetric-null center for the forward arc weight:
#'   \code{"half"} (default, \eqn{1/2}) or \code{"degree"}
#'   (\eqn{k_{\mathrm{to}}/(k_{\mathrm{from}}+k_{\mathrm{to}})}).  The degree
#'   center is the \emph{equal-distance} limit of the arc weight and is exact only
#'   when a node's neighbour distances are equal; on real graphs it is unsafe,
#'   because directional gene flow reshapes node degree along the axis, so
#'   centering on the observed degree subtracts part of the signal (it can invert
#'   the sign).  The operative symmetric-null center is what a symmetric process
#'   produces, which is close to \eqn{1/2} when the null topology has balanced
#'   degree; \code{"half"} is therefore the recommended default and
#'   \code{"degree"} is retained only for diagnostics.
#' @param test Nonparametric one-sample test of the signed departures:
#'   \code{"signed_rank"} (default, Wilcoxon signed-rank; uses magnitudes) or
#'   \code{"sign"} (binomial sign test; assumption-light).
#' @param alternative Direction of the alternative hypothesis: \code{"greater"}
#'   (default; net \emph{forward} bias along increasing \code{orientation}),
#'   \code{"less"}, or \code{"two.sided"}.
#' @param subset Optional logical or integer vector selecting a subset of edges
#'   (an induced subgraph) to test, indexed over \code{igraph::E(graph)}; the
#'   default uses every orientable edge.
#'
#' @return A list with:
#'   \describe{
#'     \item{\code{test}}{A one-row \code{data.frame}: \code{n_edges} tested,
#'       \code{n_forward} (departures above center), \code{estimate} (median
#'       signed departure), \code{statistic}, \code{p_value}, \code{method},
#'       \code{alternative}, and \code{center}.}
#'     \item{\code{edges}}{A per-edge \code{data.frame} with the forward
#'       orientation (\code{from}, \code{to}), \code{w_forward}, \code{center},
#'       \code{deviation}, and endpoint degrees \code{k_from}, \code{k_to}.}
#'   }
#'
#' @seealso \code{\link{graph_asymmetries}} for the arc weights;
#'   \code{\link{asymmetry_significance}} for per-edge resampling nulls.
#'
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#'
#' @examples
#' library(igraph)
#' A <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
#' A["A","B"] <- A["B","A"] <- 2.1
#' A["B","C"] <- A["C","B"] <- 1.2
#' A["C","D"] <- A["D","C"] <- 2.8
#' g <- graph_from_adjacency_matrix(A, mode = "undirected",
#'                                  weighted = TRUE, diag = FALSE)
#' class(g) <- c("popgraph", class(g))
#' # Stepping-stone axis: A < B < C < D
#' directional_test(g, orientation = c(A = 1, B = 2, C = 3, D = 4))$test
#'
#' @importFrom igraph is_igraph is_directed E V degree as_edgelist
#' @importFrom stats wilcox.test binom.test median
#' @export
directional_test <- function(graph,
                             orientation,
                             center      = c("half", "degree"),
                             test        = c("signed_rank", "sign"),
                             alternative = c("greater", "two.sided", "less"),
                             subset      = NULL) {

  center      <- match.arg(center)
  test        <- match.arg(test)
  alternative <- match.arg(alternative)

  if (!igraph::is_igraph(graph))   stop("'graph' must be an igraph/popgraph object")
  if (igraph::is_directed(graph))  stop("'graph' must be undirected")
  if (is.null(igraph::E(graph)$weight))
    stop("'graph' must have a numeric 'weight' edge attribute")

  if (is.null(igraph::E(graph)$w_away) || is.null(igraph::E(graph)$w_to))
    graph <- graph_asymmetries(graph)

  nodes <- igraph::V(graph)$name
  if (is.null(nodes)) {
    nodes <- as.character(seq_len(igraph::vcount(graph)))
    igraph::V(graph)$name <- nodes
  }

  # Resolve the orientation vector to one value per node, in node order.
  if (!is.null(names(orientation))) {
    pos <- orientation[nodes]
  } else {
    if (length(orientation) != length(nodes))
      stop("'orientation' must be named by node or have one entry per vertex.")
    pos <- orientation
  }
  pos <- as.numeric(pos)
  names(pos) <- nodes
  if (any(is.na(pos)))
    stop("'orientation' has no value for one or more nodes.")

  el  <- igraph::as_edgelist(graph, names = TRUE)
  u   <- el[, 1]; v <- el[, 2]
  wa  <- igraph::E(graph)$w_away          # u -> v
  wt  <- igraph::E(graph)$w_to            # v -> u
  deg <- igraph::degree(graph); names(deg) <- nodes

  prop_uv  <- wa / (wa + wt)               # forward share if u is upstream
  forward  <- pos[u] < pos[v]              # TRUE: forward is u -> v
  ku <- deg[u]; kv <- deg[v]

  w_forward  <- ifelse(forward, prop_uv, 1 - prop_uv)
  from_fwd   <- ifelse(forward, u, v)
  to_fwd     <- ifelse(forward, v, u)
  k_from     <- ifelse(forward, ku, kv)
  k_to       <- ifelse(forward, kv, ku)
  ctr        <- if (center == "half") rep(0.5, length(w_forward)) else k_to / (k_from + k_to)

  keep <- (pos[u] != pos[v]) & is.finite(w_forward) & is.finite(ctr)
  if (!is.null(subset)) {
    sel <- rep(FALSE, length(keep))
    sel[subset] <- TRUE
    keep <- keep & sel
  }

  edges <- data.frame(
    from      = from_fwd[keep],
    to        = to_fwd[keep],
    w_forward = w_forward[keep],
    center    = ctr[keep],
    deviation = (w_forward - ctr)[keep],
    k_from    = k_from[keep],
    k_to      = k_to[keep],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  dev    <- edges$deviation
  n      <- length(dev)
  n_fwd  <- sum(dev > 0)
  est    <- if (n > 0) stats::median(dev) else NA_real_

  # Degenerate case: no orientable edges, or all departures vanish.
  if (n == 0L || all(abs(dev) < 1e-12)) {
    res <- list(statistic = NA_real_, p.value = 1,
                method = if (test == "sign") "Sign test" else "Wilcoxon signed-rank")
  } else if (test == "sign") {
    n_eff <- sum(dev != 0)
    bt    <- stats::binom.test(n_fwd, n_eff, p = 0.5, alternative = alternative)
    res   <- list(statistic = n_fwd, p.value = bt$p.value, method = "Sign test")
  } else {
    wt_res <- suppressWarnings(
      stats::wilcox.test(dev, mu = 0, alternative = alternative))
    res    <- list(statistic = unname(wt_res$statistic), p.value = wt_res$p.value,
                   method = "Wilcoxon signed-rank")
  }

  test_df <- data.frame(
    n_edges     = n,
    n_forward   = n_fwd,
    estimate    = est,
    statistic   = res$statistic,
    p_value     = res$p.value,
    method      = res$method,
    alternative = alternative,
    center      = center,
    stringsAsFactors = FALSE
  )

  list(test = test_df, edges = edges)
}
