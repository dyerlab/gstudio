
#' Source-sink gradient test
#'
#' @description
#' Tests whether populations' source-sink scores \eqn{S} change monotonically
#' along a hypothesized covariate \eqn{x} (for example position along a suspected
#' axis of flow), as expected if gene flow runs consistently in one direction
#' along it.  The statistic is Spearman's \eqn{r(S, x)}; because high \eqn{S}
#' marks a source, \eqn{r < 0} places the sources at low \eqn{x} (upstream, when
#' \eqn{x} increases downstream).
#'
#' @details
#' \strong{Null.} \eqn{S} inherits smooth, node-level structure from drift on the
#' graph, so a plain permutation of \eqn{S} across populations is anti-conservative.
#' The default null (\code{null = "graph"}) is Moran spectral randomization
#' (\code{\link{msr_null}}) on the census's own Population Graph, with adjacency
#' weights \eqn{\exp(-e_{ij}^2 / 2\bar{b}^2)} where \eqn{\bar{b}} is the mean edge
#' weight.  This reproduces the smoothness \eqn{S} inherits from the graph's
#' topology and held its false-positive rate at or below nominal in simulations.
#' \code{null = "permute"} permutes \eqn{S} across populations;
#' \code{null = "adjacency"} randomizes on a user-supplied adjacency (for example
#' the true landscape adjacency).  The two-sided p-value is
#' \eqn{(1 + \#\{|r_{null}| \ge |r_{obs}|\}) / (1 + B)}.
#'
#' \strong{Ties.} \eqn{S} is rounded to 12 significant digits before ranking, so
#' floating-point near-ties between structurally equivalent populations do not
#' change the ranks.
#'
#' \strong{Bandwidth.} The default \code{gamma = 0.5} (degree-neutral) is
#' recommended for this test; see \code{\link{source_sink_scores}}.  Report the
#' degree share returned with the result.
#'
#' Graphs with more than one connected component are randomized as whole graphs.
#'
#' @param graph An undirected \code{popgraph}/\code{igraph} with a numeric
#'   \code{weight} edge attribute.
#' @param x Numeric covariate, one value per node (named by node, or in vertex order).
#' @param gamma Bandwidth multiplier for \eqn{S} (default \code{0.5}).
#' @param nperm Number of null replicates (default 999).
#' @param null \code{"graph"} (default), \code{"permute"} or \code{"adjacency"}.
#' @param adjacency For \code{null = "adjacency"}: an \eqn{n \times n} weight matrix
#'   in vertex order (or with node dimnames).
#' @param interior If \code{TRUE}, the statistic and the null replicates are
#'   restricted to interior nodes: degree greater than one and, for the
#'   observed range of \code{x}, more than \code{edge_margin} from either end.
#' @param edge_margin Margin used by \code{interior} (default 2, in units of \code{x}).
#' @param return_null If \code{TRUE}, the null correlations are returned.
#' @return An object of class \code{c("source_sink_test", "htest")}, which
#'   prints as a standard test and works with \code{broom::tidy()}:
#'   \code{statistic} (\code{r}, Spearman \eqn{r(S, x)}), \code{parameter}
#'   (\code{nodes} used and \code{nperm}), \code{p.value} (two-sided),
#'   \code{estimate} (the \code{degree share}), \code{null.value},
#'   \code{alternative}, \code{method}, \code{data.name}, plus \code{null}
#'   (the null model used), \code{gamma}, \code{scores} (from
#'   \code{\link{source_sink_scores}}) and, with \code{return_null = TRUE},
#'   \code{r_null} (the null distribution of \eqn{r}).
#' @seealso \code{\link{source_sink_scores}}, \code{\link{msr_null}},
#'   \code{\link{ibgd}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' library(igraph)
#' set.seed(2)
#' K <- 10
#' A <- matrix(0, K, K, dimnames = list(paste0("P", 1:K), paste0("P", 1:K)))
#' for (i in 1:(K - 1)) A[i, i + 1] <- A[i + 1, i] <- runif(1, 0.5, 2)
#' A[2, 5] <- A[5, 2] <- 2.5; A[6, 9] <- A[9, 6] <- 2.2
#' g <- graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE)
#' source_sink_test(g, x = 1:K, nperm = 199)
#' @export
source_sink_test <- function(graph, x, gamma = 0.5, nperm = 999L, null = c("graph", "permute", "adjacency"),
                             adjacency = NULL, interior = FALSE, edge_margin = 2, return_null = FALSE) {
  null <- match.arg(null)
  dname <- paste(deparse1(substitute(graph)), "and", deparse1(substitute(x)))
  graph <- .gravity_check(graph)
  nodes <- igraph::V(graph)$name
  xv <- .gravity_x(x, nodes)
  sc <- source_sink_scores(graph, gamma)
  S <- signif(sc$S, 12)
  keep <- rep(TRUE, length(S))
  if (interior) keep <- sc$degree > 1 & xv > min(xv) + edge_margin & xv < max(xv) - edge_margin
  if (sum(keep) < 4) stop("fewer than 4 nodes available for the test")
  r_obs <- stats::cor(S[keep], xv[keep], method = "spearman")
  draws <- switch(null,
    permute = replicate(nperm, sample(S)),
    graph = {
      A <- igraph::as_adjacency_matrix(graph, attr = "weight", sparse = FALSE)[nodes, nodes]
      bbar <- mean(igraph::E(graph)$weight)
      A[A > 0] <- exp(-A[A > 0]^2 / (2 * bbar^2))
      msr_null(S, A, nperm) },
    adjacency = {
      if (is.null(adjacency)) stop("null = 'adjacency' needs an 'adjacency' matrix")
      A <- as.matrix(adjacency)
      if (!is.null(rownames(A))) A <- A[nodes, nodes]
      msr_null(S, A, nperm) })
  r_null <- as.numeric(stats::cor(draws[keep, , drop = FALSE], xv[keep], method = "spearman"))
  p <- (1 + sum(abs(r_null) >= abs(r_obs) - 1e-12)) / (nperm + 1)
  null_label <- c(graph = "Moran spectral randomization on the graph",
                  permute = "permutation of S", adjacency = "Moran spectral randomization on the supplied adjacency")[[null]]
  out <- list(statistic   = c(r = r_obs),
              parameter   = c(nodes = sum(keep), nperm = as.integer(nperm)),
              p.value     = p,
              estimate    = c(`degree share` = attr(sc, "degree_share")),
              null.value  = c(r = 0),
              alternative = "two.sided",
              method      = sprintf("Source-sink gradient test (Spearman r(S, x); %s)", null_label),
              data.name   = dname,
              null        = null,
              gamma       = gamma,
              scores      = sc)
  if (return_null) out$r_null <- r_null
  class(out) <- c("source_sink_test", "htest")
  out
}

#' @rdname source_sink_test
#' @param ... Passed to \code{print.htest()}.
#' @export
print.source_sink_test <- function(x, ...) {
  NextMethod()
  cat(sprintf("Bandwidth gamma = %g.  Sources at %s values of x.\n\n",
              x$gamma, if (unname(x$statistic) < 0) "low" else "high"))
  invisible(x)
}
