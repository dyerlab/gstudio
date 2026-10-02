
#' Directional isolation by graph distance
#'
#' @description
#' Asks whether modelling direction explicitly improves how well a Population Graph
#' explains isolation by distance.  Standard isolation by graph distance (IBGD)
#' regresses all-pairs conditional genetic distance (cGD) on spatial separation
#' (\eqn{R^2_{cGD}}).  The directional model regresses all-pairs directed pGD
#' (\code{\link{pgd}}) on separation with separate forward and reverse slopes,
#' \eqn{pGD_{ij} \sim d_{ij} + d_{ij} r_{ij}}, where \eqn{r_{ij} = 1} for reverse
#' (upstream) pairs (adjusted \eqn{R^2_{pGD}}).  \eqn{\Delta R^2 = R^2_{pGD} -
#' R^2_{cGD} > 0} favours the directional description, consistent with asymmetric
#' gene flow along the hypothesized axis; \eqn{\Delta R^2 \le 0} is an absence of
#' evidence for asymmetry, not evidence of symmetry.  The forward-to-reverse slope
#' ratio \eqn{b_{fwd}/b_{rev}} describes a detected asymmetry: below one means
#' genetic distance accumulates more slowly downstream.
#'
#' @details
#' The two fits use different response matrices, so the comparison is not a nested
#' model test; its error rates were characterized by simulation (anti-conservative
#' under strong drift, where a directional fit should be corroborated by
#' \code{\link{source_sink_test}}).
#'
#' @param graph An undirected \code{popgraph}/\code{igraph} with a numeric
#'   \code{weight} edge attribute.
#' @param x Numeric position along the hypothesized axis, one value per node (named
#'   or in vertex order).  Separation is \eqn{|x_j - x_i|} and a pair is forward when
#'   \eqn{x_j > x_i}.  Alternatively supply \code{distance} and \code{forward}.
#' @param distance Optional \eqn{n \times n} spatial (or resistance) distance matrix,
#'   used instead of \eqn{|x_j - x_i|}.
#' @param forward Optional logical \eqn{n \times n} matrix, \code{TRUE} where the
#'   ordered pair \eqn{(i, j)} runs in the hypothesized forward direction; required
#'   with \code{distance}.
#' @param gamma Bandwidth multiplier (default \code{0.5}).
#' @return A one-row \code{data.frame}: \code{r2_cgd}, \code{r2_pgd_blind} (pGD with
#'   one shared slope), \code{r2_pgd_dir} (adjusted), \code{delta_r2},
#'   \code{b_fwd}, \code{b_rev}, \code{slope_ratio}, \code{preferred}
#'   (\code{delta_r2 > 0}), \code{gamma}.
#' @seealso \code{\link{pgd}}, \code{\link{source_sink_test}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' library(igraph)
#' set.seed(3)
#' K <- 8
#' A <- matrix(0, K, K, dimnames = list(paste0("P", 1:K), paste0("P", 1:K)))
#' for (i in 1:(K - 1)) A[i, i + 1] <- A[i + 1, i] <- runif(1, 0.5, 2)
#' A[1, 3] <- A[3, 1] <- 2.4; A[4, 7] <- A[7, 4] <- 2.6
#' g <- graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE)
#' directional_ibgd(g, x = 1:K)
#' @export
directional_ibgd <- function(graph, x = NULL, distance = NULL, forward = NULL, gamma = 0.5) {
  graph <- .gravity_check(graph)
  nodes <- igraph::V(graph)$name; n <- length(nodes)
  if (!is.null(distance)) {
    X <- as.matrix(distance); if (!is.null(rownames(X))) X <- X[nodes, nodes]
    if (is.null(forward)) stop("'forward' is required with 'distance'")
    Fw <- as.matrix(forward); if (!is.null(rownames(Fw))) Fw <- Fw[nodes, nodes]
  } else {
    xv <- .gravity_x(x, nodes)
    X <- abs(outer(xv, xv, "-")); Fw <- outer(xv, xv, function(a, b) b > a)
  }
  C <- igraph::distances(graph, v = nodes, to = nodes, weights = igraph::E(graph)$weight)
  P <- pgd(graph, gamma, output = "matrix")
  up <- upper.tri(C); off <- row(P) != col(P)
  ok_u <- up & is.finite(C) & is.finite(P) & is.finite(t(P))
  ok_o <- off & is.finite(P) & (Fw | t(Fw))
  d_o <- data.frame(p = P[ok_o], X = X[ok_o], rev = as.numeric(!Fw[ok_o]))
  fit <- stats::lm(p ~ X + X:rev, d_o); cf <- stats::coef(fit)
  r2c <- stats::cor(C[ok_u], X[ok_u])^2
  r2d <- summary(fit)$adj.r.squared
  data.frame(r2_cgd = r2c, r2_pgd_blind = summary(stats::lm(p ~ X, d_o))$adj.r.squared, r2_pgd_dir = r2d,
             delta_r2 = r2d - r2c, b_fwd = unname(cf["X"]), b_rev = unname(cf["X"] + cf["X:rev"]),
             slope_ratio = unname(cf["X"] / (cf["X"] + cf["X:rev"])), preferred = r2d > r2c, gamma = gamma)
}
