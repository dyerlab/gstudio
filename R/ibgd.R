#' Isolation by graph distance
#'
#' @description
#' Tests how well a Population Graph's genetic distances are explained by
#' separation along a spatial (or hypothesized) axis, in one of two forms:
#' \describe{
#'   \item{\code{"cgd"}}{(default) \strong{Isolation by graph distance (IBGD).}
#'     All-pairs conditional genetic distance (cGD, the shortest-path distance
#'     over edge weights) against spatial separation.  The statistic is the
#'     correlation \eqn{r} between the two over population pairs, tested with a
#'     Mantel permutation test: populations' positions are permuted and \eqn{r}
#'     recomputed, with the one-sided p-value
#'     \eqn{(1 + \#\{r_{null} \ge r_{obs}\}) / (1 + B)}.}
#'   \item{\code{"pgd"}}{\strong{Directional IBGD.}  Asks whether modelling
#'     direction improves the fit.  All-pairs directed pGD (\code{\link{pgd}}) is
#'     regressed on separation with separate forward and reverse slopes,
#'     \eqn{pGD_{ij} \sim d_{ij} + d_{ij} r_{ij}}, where \eqn{r_{ij} = 1} for
#'     reverse (upstream) pairs, giving adjusted \eqn{R^2_{pGD}}.  The statistic is
#'     \eqn{\Delta R^2 = R^2_{pGD} - R^2_{cGD}}; \eqn{\Delta R^2 > 0} favours the
#'     directional description, consistent with asymmetric gene flow along the
#'     axis, and \eqn{\Delta R^2 \le 0} is an absence of evidence for asymmetry,
#'     not evidence of symmetry.  The forward-to-reverse slope ratio
#'     \eqn{b_{fwd}/b_{rev}} describes a detected asymmetry: below one means
#'     genetic distance accumulates more slowly downstream.}
#' }
#'
#' @details
#' \strong{No p-value for \code{"pgd"}.}  The two fits use different response
#' matrices, so the comparison is not a nested-model test and has no analytic
#' p-value, and permuting positions would test for any isolation by distance
#' rather than for direction.  Its error rates were characterized by simulation:
#' it is anti-conservative under strong drift, so corroborate a directional fit
#' with \code{\link{source_sink_test}}.
#'
#' \strong{Pairs.} \code{"cgd"} uses each unordered pair of connected populations
#' once; \code{"pgd"} uses every ordered pair, since pGD differs by direction.
#'
#' @param graph An undirected \code{popgraph}/\code{igraph} with a numeric
#'   \code{weight} edge attribute.
#' @param x Numeric position along the axis, one value per node (named by node,
#'   or in vertex order).  Separation is \eqn{|x_j - x_i|}, and for
#'   \code{"pgd"} a pair is forward when \eqn{x_j > x_i}.  Alternatively supply
#'   \code{distance} (and, for \code{"pgd"}, \code{forward}).
#' @param mode \code{"cgd"} (default) or \code{"pgd"}.
#' @param gamma Bandwidth multiplier for pGD (default \code{0.5}); used only by
#'   \code{"pgd"}.
#' @param nperm Number of Mantel permutations for \code{"cgd"} (default 999).
#' @param distance Optional \eqn{n \times n} spatial (or resistance) distance
#'   matrix, used instead of \eqn{|x_j - x_i|}.  When both \code{x} and
#'   \code{distance} are \code{NULL} and the graph has \code{Longitude} and
#'   \code{Latitude} vertex attributes (e.g. from
#'   \code{population_graph(decorate = TRUE)}), the great-circle distance in km
#'   between nodes is used (see \code{\link{strata_distance}}); \code{"pgd"}
#'   then still needs \code{forward}.
#' @param forward Optional logical \eqn{n \times n} matrix, \code{TRUE} where the
#'   ordered pair \eqn{(i, j)} runs in the hypothesized forward direction;
#'   required with \code{distance} for \code{"pgd"}.
#' @return An object of class \code{c("ibgd", "htest")}, which prints as a
#'   standard test and works with \code{broom::tidy()}:
#'   \describe{
#'     \item{\code{statistic}}{\code{r} (\code{"cgd"}) or \code{delta R2}
#'       (\code{"pgd"}).}
#'     \item{\code{parameter}}{Number of population pairs, plus \code{nperm}
#'       for \code{"cgd"}.}
#'     \item{\code{p.value}}{Mantel permutation p-value (\code{"cgd"} only).}
#'     \item{\code{estimate}}{\code{"cgd"}: \code{R2} and the cGD-on-separation
#'       \code{slope}.  \code{"pgd"}: \code{R2 cGD}, \code{adj R2 pGD},
#'       \code{forward slope}, \code{reverse slope} and \code{slope ratio}.}
#'     \item{\code{null.value}, \code{alternative}}{For \code{"cgd"}: \eqn{r = 0}
#'       against \eqn{r > 0}.}
#'     \item{\code{method}, \code{data.name}, \code{mode}}{Description, inputs, and
#'       the mode used; \code{"pgd"} also returns \code{gamma}.}
#'   }
#' @seealso \code{\link{pgd}}, \code{\link{source_sink_test}},
#'   \code{\link{genetic_distance}}, \code{vignette("genetic_gravity")}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' data(gravity)
#' graph <- population_graph(gravity)
#' deme  <- as.integer(sub("Pop", "", igraph::V(graph)$name))
#'
#' set.seed(1)
#' ibgd(graph, x = deme)                 # isolation by graph distance (cGD)
#' ibgd(graph, x = deme, mode = "pgd")   # does a directional model fit better?
#'
#' # Distances from node coordinates
#' data(arapat)
#' g <- population_graph(arapat, decorate = TRUE)
#' ibgd(g, nperm = 99)
#' @export
ibgd <- function(graph, x = NULL, mode = c("cgd", "pgd"), gamma = 0.5,
                 nperm = 999L, distance = NULL, forward = NULL) {
  mode  <- match.arg(mode)
  gname <- deparse1(substitute(graph))
  dname <- paste(gname, "and",
                 if (!is.null(distance)) deparse1(substitute(distance)) else deparse1(substitute(x)))
  graph <- .gravity_check(graph)
  nodes <- igraph::V(graph)$name

  # Neither positions nor distances given: fall back on node coordinates,
  # e.g. from population_graph(decorate = TRUE).
  if (is.null(distance) && is.null(x) &&
      all(c("Longitude", "Latitude") %in% igraph::vertex_attr_names(graph))) {
    xy <- data.frame(Stratum = nodes, Longitude = igraph::V(graph)$Longitude,
                     Latitude = igraph::V(graph)$Latitude, stringsAsFactors = FALSE)
    if (anyNA(xy[-1]))
      stop("Some nodes lack 'Longitude'/'Latitude'; supply 'x' or 'distance'.")
    if (mode == "pgd" && is.null(forward))
      stop("'forward' is required for mode = \"pgd\" when distances come from node coordinates")
    distance <- strata_distance(xy, mode = "Circle")
    dname <- paste(gname, "and great-circle distance (km)")
  }

  if (!is.null(distance)) {
    X <- as.matrix(distance)
    if (!is.null(rownames(X))) X <- X[nodes, nodes]
    if (mode == "pgd") {
      if (is.null(forward)) stop("'forward' is required with 'distance' for mode = \"pgd\"")
      Fw <- as.matrix(forward)
      if (!is.null(rownames(Fw))) Fw <- Fw[nodes, nodes]
    }
  } else {
    if (is.null(x)) stop("Supply 'x' (positions) or 'distance', or give the graph 'Longitude' and 'Latitude' vertex attributes.")
    xv <- .gravity_x(x, nodes)
    X  <- abs(outer(xv, xv, "-"))
    Fw <- outer(xv, xv, function(a, b) b > a)
  }

  C  <- igraph::distances(graph, v = nodes, to = nodes, weights = igraph::E(graph)$weight)
  ok <- upper.tri(C) & is.finite(C)
  r_obs <- stats::cor(C[ok], X[ok])

  if (mode == "cgd") {
    nperm <- as.integer(nperm)
    r_null <- vapply(seq_len(nperm), function(b) {
      idx <- sample.int(length(nodes))
      stats::cor(C[ok], X[idx, idx][ok])
    }, numeric(1))
    fit <- stats::lm(C[ok] ~ X[ok])
    res <- list(
      statistic   = c(r = r_obs),
      parameter   = c(pairs = sum(ok), nperm = nperm),
      p.value     = (1 + sum(r_null >= r_obs)) / (1 + nperm),
      estimate    = c(R2 = r_obs^2, slope = unname(stats::coef(fit)[2])),
      null.value  = c(r = 0),
      alternative = "greater",
      method      = "Isolation by graph distance (cGD; Mantel permutation test)",
      data.name   = dname,
      mode        = mode)
  } else {
    P    <- pgd(graph, gamma, output = "matrix")
    ok_o <- row(P) != col(P) & is.finite(P) & (Fw | t(Fw))
    d_o  <- data.frame(p = P[ok_o], X = X[ok_o], rev = as.numeric(!Fw[ok_o]))
    fit  <- stats::lm(p ~ X + X:rev, d_o)
    cf   <- stats::coef(fit)
    r2c  <- r_obs^2
    r2d  <- summary(fit)$adj.r.squared
    b_fwd <- unname(cf["X"]); b_rev <- unname(cf["X"] + cf["X:rev"])
    res <- list(
      statistic = c(`delta R2` = r2d - r2c),
      parameter = c(pairs = sum(ok_o)),
      estimate  = c(`R2 cGD` = r2c, `adj R2 pGD` = r2d, `forward slope` = b_fwd,
                    `reverse slope` = b_rev, `slope ratio` = b_fwd / b_rev),
      method    = sprintf("Directional isolation by graph distance (pGD at gamma = %g vs cGD)", gamma),
      gamma     = gamma,
      data.name = dname,
      mode      = mode)
  }
  class(res) <- c("ibgd", "htest")
  res
}

#' @rdname ibgd
#' @param ... Passed to \code{print.htest()}.
#' @export
print.ibgd <- function(x, ...) {
  NextMethod()
  if (identical(x$mode, "pgd")) {
    dr <- unname(x$statistic); ratio <- unname(x$estimate["slope ratio"])
    if (dr > 0) {
      cat(sprintf("Directional description preferred (delta R2 > 0); slope ratio %.3g: genetic\n", ratio),
          sprintf("distance accumulates more %s in the forward direction.\n",
                  if (ratio < 1) "slowly" else "quickly"), sep = "")
    } else {
      cat("Symmetric description sufficient (delta R2 <= 0): no evidence for asymmetry.\n")
    }
    cat("No p-value: the fits are not nested (see ?ibgd); corroborate with source_sink_test().\n\n")
  }
  invisible(x)
}
