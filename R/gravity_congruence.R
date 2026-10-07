#' Gravity congruence of two Population Graphs
#'
#' @description
#' Asks whether two species (or two markers, or two censuses) move genes the
#' same way across the same set of populations.  Each graph's genetic gravity
#' is computed on its own full topology (see \code{\link{gravity_field}}), and
#' the two are then compared at two levels:
#' \describe{
#'   \item{\strong{Source-sink scores, S.}}{The Spearman rank correlation of
#'     \eqn{S_1} and \eqn{S_2} over all populations.  Ranks are used because
#'     \eqn{S} is not on a common scale across graphs with different sizes and
#'     edge weights.  Each population is also classed as a concordant
#'     \code{"source"} (\eqn{S_1, S_2 > 0}), concordant \code{"sink"}
#'     (\eqn{S_1, S_2 < 0}), \code{"discordant"} (a source in one graph and
#'     a sink in the other), or \code{"neutral"} (\eqn{S = 0} in either graph,
#'     so it has no direction to agree or disagree with).  S is 0 for a
#'     population that is isolated, or in a two-node component, in that graph:
#'     a node with a single neighbour gives it all its weight, so both ends of
#'     a lone edge weigh each other equally and \eqn{\Delta = 0}.  All
#'     populations, neutral ones included, enter the correlation.}
#'   \item{\strong{Edge direction.}}{Over the edges present in both graphs (the
#'     \code{\link{congruence_topology}}), the proportion whose asymmetry
#'     \eqn{\Delta} has the same sign in both, i.e. along which gene flow runs
#'     the same way.  Edges with \eqn{\Delta = 0} in either graph have no
#'     direction and are left out.}
#' }
#'
#' \strong{Tests.}  The S correlation is tested by permuting the second graph's
#' population labels, which keeps each graph's topology intact while breaking
#' the correspondence between them (relabelling a graph just permutes its
#' \eqn{S} values), with one-sided p-value
#' \eqn{(1 + \#\{\rho_{null} \ge \rho_{obs}\}) / (1 + B)}.  Edge direction is
#' tested with an exact binomial (sign) test of the number of concordant edges
#' against 1/2, conditional on which edges are shared: a label permutation is
#' not used here because shuffled graphs share only a few edges, whose
#' concordance proportion is then too noisy to serve as a null.  The sign test
#' treats edges as independent, so read small p-values from densely connected
#' graphs with some caution; a 1/2 null suits the degree-neutral default
#' \code{gamma = 0.5}, where \eqn{\Delta} is not driven by degree.
#'
#' @param x,y Two undirected \code{popgraph}/\code{igraph} objects with a numeric
#'   \code{weight} edge attribute and \strong{the same set of populations}, or
#'   two single-panel \code{\link{gravity_field}} objects (their \code{gamma} is
#'   then used).
#' @param gamma Bandwidth multiplier (default \code{0.5}), used for graphs; see
#'   \code{\link{source_sink_scores}}.
#' @param nperm Number of label permutations for the S test (default 999).
#' @return An object of class \code{"gravity_congruence"}: a list with
#'   \describe{
#'     \item{\code{nodes}}{\code{data.frame}: \code{Stratum}, \code{S.1},
#'       \code{S.2}, \code{gravity.1}, \code{gravity.2}, \code{degree.1},
#'       \code{degree.2}, \code{concordance}, followed by the populations'
#'       other vertex attributes (combined as in \code{\link{congruence_topology}}).}
#'     \item{\code{edges}}{\code{data.frame}, one row per shared edge:
#'       \code{from}, \code{to}, \code{weight.1}, \code{weight.2},
#'       \code{delta.1}, \code{delta.2} (each \eqn{\Delta_{from \to to}}),
#'       \code{concordant} (\code{NA} when either \eqn{\Delta} is 0).}
#'     \item{\code{tests}}{\code{data.frame}, one row per test (\code{"S"},
#'       \code{"edge direction"}): \code{test}, \code{statistic} (Spearman
#'       \eqn{\rho}, or the proportion of concordant edges), \code{parameter}
#'       (populations, or shared edges with a direction), \code{p.value},
#'       \code{nperm} (\code{NA} for the sign test), \code{method},
#'       \code{alternative}.}
#'     \item{\code{graphs}}{\code{congruence}: the congruence topology with the
#'       node and edge columns above as attributes; \code{graph1}, \code{graph2}:
#'       the input graphs with \code{S}, \code{gravity} and \code{delta}
#'       attributes as from \code{\link{gravity_field}}.}
#'     \item{\code{gamma}, \code{data.name}}{The bandwidths used, and the inputs.}
#'   }
#'   Methods: \code{print()}, \code{plot()} (see
#'   \code{\link{plot.gravity_congruence}}) and \code{as.data.frame(what =)}.
#' @seealso \code{\link{gravity_field}}, \code{\link{congruence_topology}},
#'   \code{\link{test_congruence}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' library(igraph)
#' set.seed(2)
#' K <- 10
#' nm <- paste0("P", 1:K)
#' chain <- function(extra) {
#'   A <- matrix(0, K, K, dimnames = list(nm, nm))
#'   for (i in 1:(K - 1)) A[i, i + 1] <- A[i + 1, i] <- runif(1, 0.5, 2)
#'   for (e in extra) A[e[1], e[2]] <- A[e[2], e[1]] <- runif(1, 2, 3)
#'   graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE)
#' }
#' g1 <- chain(list(c(2, 5), c(6, 9)))
#' g2 <- chain(list(c(2, 5), c(3, 8)))
#' gc <- gravity_congruence(g1, g2, nperm = 199)
#' gc
#' as.data.frame(gc, what = "tests")
#' plot(gc, layout = cbind(x = 1:K, y = 2 * sin(1:K * pi / 5)))
#' @export
gravity_congruence <- function(x, y, gamma = 0.5, nperm = 999L) {
  dname <- paste(deparse1(substitute(x)), "and", deparse1(substitute(y)))
  in1 <- .congruence_input(x, gamma, "x")
  in2 <- .congruence_input(y, gamma, "y")
  g1 <- in1$graph; g2 <- in2$graph

  nodes <- igraph::V(g1)$name
  n2 <- igraph::V(g2)$name
  if (anyDuplicated(nodes) || anyDuplicated(n2) || !setequal(nodes, n2)) {
    only <- c(setdiff(nodes, n2), setdiff(n2, nodes))
    stop("gravity_congruence() needs two graphs with exactly the same populations",
         if (length(only)) paste0("; found in only one: ",
                                  paste(utils::head(only, 10), collapse = ", "),
                                  if (length(only) > 10) ", ..."),
         ".  Use congruence_topology() to compare graphs on different node sets.")
  }
  K <- length(nodes)

  side <- function(g, gm) {
    sc <- source_sink_scores(g, gm)
    ed <- gravity_edges(g, gm)
    D <- W <- matrix(NA_real_, K, K, dimnames = list(nodes, nodes))
    D[cbind(ed$from, ed$to)] <- ed$Delta; D[cbind(ed$to, ed$from)] <- -ed$Delta
    W[cbind(ed$from, ed$to)] <- W[cbind(ed$to, ed$from)] <- ed$weight
    i <- match(nodes, sc$Stratum)
    g <- igraph::set_vertex_attr(g, "S", value = sc$S)
    g <- igraph::set_vertex_attr(g, "gravity", value = sc$gravity)
    g <- igraph::set_edge_attr(g, "delta", value = ed$Delta)
    list(graph = g, S = sc$S[i], gravity = sc$gravity[i], degree = sc$degree[i], D = D, W = W)
  }
  s1 <- side(g1, in1$gamma)
  s2 <- side(g2, in2$gamma)

  # Shared edges, oriented row -> column of the upper triangle.
  sh <- which(upper.tri(s1$D) & !is.na(s1$D) & !is.na(s2$D), arr.ind = TRUE)
  sh <- sh[order(sh[, 1], sh[, 2]), , drop = FALSE]
  d1 <- s1$D[sh]; d2 <- s2$D[sh]
  tiny <- 1e-12
  conc <- ifelse(abs(d1) < tiny | abs(d2) < tiny, NA, sign(d1) == sign(d2))

  rho_obs <- stats::cor(s1$S, s2$S, method = "spearman")
  nperm <- as.integer(nperm)
  rho_null <- vapply(seq_len(nperm), function(b)
    stats::cor(s1$S, s2$S[sample.int(K)], method = "spearman"), numeric(1))
  p_rho <- if (is.na(rho_obs)) NA_real_ else (1 + sum(rho_null >= rho_obs - 1e-12, na.rm = TRUE)) /
    (1 + sum(!is.na(rho_null)))
  n_dir <- sum(!is.na(conc))
  k_dir <- sum(conc, na.rm = TRUE)
  edge_obs <- if (n_dir) k_dir / n_dir else NA_real_
  p_edge <- if (n_dir) stats::binom.test(k_dir, n_dir, 0.5, alternative = "greater")$p.value else NA_real_

  tests <- data.frame(
    test        = c("S", "edge direction"),
    statistic   = c(rho_obs, edge_obs),
    parameter   = c(K, n_dir),
    p.value     = c(p_rho, p_edge),
    nperm       = c(nperm, NA_integer_),
    method      = c("Spearman correlation of source-sink scores (label permutation test)",
                    "Proportion of shared edges with concordant gene-flow direction (exact sign test)"),
    alternative = "greater",
    stringsAsFactors = FALSE)

  concordance <- ifelse(abs(s1$S) < tiny | abs(s2$S) < tiny, "neutral",
                        ifelse(s1$S > 0 & s2$S > 0, "source",
                               ifelse(s1$S < 0 & s2$S < 0, "sink", "discordant")))
  nd <- data.frame(Stratum = nodes, S.1 = s1$S, S.2 = s2$S,
                   gravity.1 = s1$gravity, gravity.2 = s2$gravity,
                   degree.1 = s1$degree, degree.2 = s2$degree,
                   concordance = concordance, stringsAsFactors = FALSE)
  ed <- data.frame(from = nodes[sh[, 1]], to = nodes[sh[, 2]],
                   weight.1 = s1$W[sh], weight.2 = s2$W[sh],
                   delta.1 = d1, delta.2 = d2, concordant = conc,
                   stringsAsFactors = FALSE)

  # Congruence topology carrying the parents' decorations and the comparison.
  computed <- c("S", "gravity")
  strip <- function(g) {
    for (a in intersect(computed, igraph::vertex_attr_names(g))) g <- igraph::delete_vertex_attr(g, a)
    g
  }
  cong <- igraph::graph_from_data_frame(ed, directed = FALSE,
                                        vertices = data.frame(name = nodes, stringsAsFactors = FALSE))
  cong <- .congruence_vertex_attrs(cong, strip(g1), strip(g2))
  nd <- cbind(nd, .extra_attrs(igraph::vertex_attr(cong), K, c(names(nd), "name")))
  for (a in setdiff(names(nd), "Stratum")) cong <- igraph::set_vertex_attr(cong, a, value = nd[[a]])
  class(cong) <- c("popgraph", "igraph")

  out <- list(nodes = nd, edges = ed, tests = tests,
              graphs = list(congruence = cong, graph1 = s1$graph, graph2 = s2$graph),
              gamma = c(in1$gamma, in2$gamma), data.name = dname)
  class(out) <- "gravity_congruence"
  out
}

# A graph (with the default gamma) or a single-panel gravity field (with its own).
#' @keywords internal
#' @noRd
.congruence_input <- function(x, gamma, arg) {
  if (inherits(x, "gravity_field")) {
    if (length(x$graphs) != 1L)
      stop(sprintf("'%s' is a multi-panel gravity field; pass a single panel or a graph.", arg))
    return(list(graph = .gravity_check(x$graphs[[1]]), gamma = x$diagnostics$gamma))
  }
  if (!igraph::is_igraph(x))
    stop(sprintf("'%s' must be a popgraph/igraph or a gravity_field.", arg))
  list(graph = .gravity_check(x), gamma = gamma)
}

#' @rdname gravity_congruence
#' @param n Number of populations to list per class in \code{print()} (default 5).
#' @param ... Ignored.
#' @export
print.gravity_congruence <- function(x, n = 5, ...) {
  t <- x$tests
  gm <- if (x$gamma[1] == x$gamma[2]) sprintf("gamma = %g", x$gamma[1])
        else sprintf("gamma = %g / %g", x$gamma[1], x$gamma[2])
  cat(sprintf("Gravity congruence: %s\n", x$data.name))
  cat(sprintf("  %d populations, %d shared edges (of %d and %d), %s\n",
              nrow(x$nodes), nrow(x$edges), igraph::ecount(x$graphs$graph1),
              igraph::ecount(x$graphs$graph2), gm))
  fp <- function(p) if (is.na(p)) "NA" else format.pval(p, digits = 3)
  cat(sprintf("  S:              rho = %.3f, p = %s  [%d permutations]\n",
              t$statistic[1], fp(t$p.value[1]), t$nperm[1]))
  cat(sprintf("  edge direction: %.3f concordant (%d of %d), p = %s  [sign test]\n",
              t$statistic[2], sum(x$edges$concordant, na.rm = TRUE), as.integer(t$parameter[2]),
              fp(t$p.value[2])))
  lst <- function(cl) {
    v <- x$nodes$Stratum[x$nodes$concordance == cl]
    if (!length(v)) return("none")
    paste0(paste(utils::head(v, n), collapse = ", "), if (length(v) > n) sprintf(", ... (%d)", length(v)))
  }
  cat(sprintf("  shared sources: %s\n", lst("source")))
  cat(sprintf("  shared sinks:   %s\n", lst("sink")))
  cat(sprintf("  discordant:     %s\n", lst("discordant")))
  if (any(x$nodes$concordance == "neutral"))
    cat(sprintf("  neutral:        %s\n", lst("neutral")))
  invisible(x)
}

#' @rdname gravity_congruence
#' @param row.names,optional Ignored.
#' @param what \code{"nodes"} (default), \code{"edges"} or \code{"tests"}.
#' @export
as.data.frame.gravity_congruence <- function(x, row.names = NULL, optional = FALSE,
                                             what = c("nodes", "edges", "tests"), ...) {
  x[[match.arg(what)]]
}
