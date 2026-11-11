#' Isolation by distance among strata
#'
#' @description
#' The single interface for testing whether genetic differentiation among
#' strata increases with their separation, starting from raw genotypes.
#' \describe{
#'   \item{\code{"IBD"}}{(default) \strong{Isolation by distance.}  Pairwise
#'     genetic distance among strata (\code{\link{genetic_distance}}, Nei's by
#'     default; see \code{genetic}) against spatial separation, as the
#'     correlation \eqn{r} over pairs of strata tested with a one-sided Mantel
#'     permutation test.}
#'   \item{\code{"IBGD"}}{\strong{Isolation by graph distance.}  A Population
#'     Graph is built with \code{\link{population_graph}} and its conditional
#'     genetic distance (cGD) is tested against separation the same way; see
#'     \code{\link{ibgd}} (\code{mode = "cgd"}).}
#'   \item{\code{"PGD"}}{\strong{Directional isolation by graph distance.}  Asks
#'     whether partitioned cGD with separate forward and reverse slopes fits
#'     better than cGD, \eqn{\Delta R^2}; see \code{\link{ibgd}}
#'     (\code{mode = "pgd"}).  It needs a direction: give \code{axis} or
#'     \code{forward}.  It has no p-value.}
#' }
#' If you already have a Population Graph, call \code{\link{ibgd}} on it
#' directly.
#'
#' @details
#' \strong{Separation} among strata comes from, in order of precedence:
#' \code{spatial} (any distance matrix among strata, e.g. resistance); the
#' \code{axis} positions, as \eqn{|a_j - a_i|}; or the great-circle distance
#' in km between the mean \code{Longitude}/\code{Latitude} of each stratum
#' (\code{\link{strata_coordinates}}, \code{\link{strata_distance}}).
#'
#' @param x A \code{data.frame} with \code{\link{locus}} columns.
#' @param stratum The column holding strata (default \code{"Population"}), or
#'   taken from \code{dplyr::group_by()} when \code{x} is grouped.
#' @param mode \code{"IBD"} (default), \code{"IBGD"} or \code{"PGD"}.
#' @param genetic The \code{\link{genetic_distance}} mode used by \code{"IBD"}
#'   (default \code{"Nei"}); it must be a stratum-level distance.
#' @param spatial Optional distance matrix among strata, with row and column
#'   names matching the strata.
#' @param axis Optional position of each stratum along a hypothesized axis
#'   (e.g. a river or a latitudinal gradient): the name of a numeric column of
#'   \code{x} (averaged within strata) or a numeric vector named by stratum.
#'   For \code{"PGD"}, a pair runs forward when \eqn{a_j > a_i}.
#' @param forward For \code{"PGD"} without \code{axis}, a logical matrix among
#'   strata, \code{TRUE} where the ordered pair \eqn{(i, j)} runs forward.
#' @param nperm Number of Mantel permutations (default 999); not used by
#'   \code{"PGD"}.
#' @param gamma Bandwidth multiplier for pGD (default \code{0.5}); \code{"PGD"}
#'   only.
#' @param ... Passed to \code{\link{genetic_distance}} (\code{"IBD"}) or
#'   \code{\link{population_graph}} (\code{"IBGD"}, \code{"PGD"}).
#' @return An object of class \code{"htest"} that prints as a standard test and
#'   works with \code{broom::tidy()}: the correlation \code{r} as
#'   \code{statistic}, the number of pairs and permutations as
#'   \code{parameter}, the Mantel \code{p.value}, and \code{R2} and the
#'   \code{slope} of genetic distance on separation as \code{estimate}.  For
#'   \code{"IBGD"} and \code{"PGD"} it is the \code{\link{ibgd}} result.
#' @seealso \code{\link{ibgd}}, \code{\link{pgd}}, \code{\link{genetic_distance}},
#'   \code{\link{strata_distance}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#' data(arapat)
#' set.seed(1)
#' genetic_isolation(arapat, nperm = 99)
#' genetic_isolation(arapat, mode = "IBGD", nperm = 99)
#'
#' # Along an axis, which also gives the direction for PGD
#' data(gravity)
#' pops <- unique(gravity$Population)
#' deme <- setNames(as.integer(sub("Pop", "", pops)), pops)
#' genetic_isolation(gravity, mode = "IBGD", axis = deme, nperm = 99)
#' genetic_isolation(gravity, mode = "PGD", axis = deme)
genetic_isolation <- function( x, stratum = "Population", mode = c("IBD", "IBGD", "PGD"),
                               genetic = "Nei", spatial = NULL, axis = NULL, forward = NULL,
                               nperm = 999L, gamma = 0.5, ... ) {
  dname <- deparse1(substitute(x))
  mode <- toupper(mode[1])
  if( !(mode %in% c("IBD", "IBGD", "PGD")) )
    stop("mode must be one of 'IBD', 'IBGD' or 'PGD'.")
  if( !is(x, "data.frame") || length(column_class(x, "locus")) < 1 )
    stop("genetic_isolation() takes a data.frame with locus columns.")
  stratum <- .detect_stratum(x, stratum, default = "Population", explicit = !missing(stratum))
  x <- .plain_df(x)
  if( !(stratum %in% names(x)) )
    stop("Requested stratum is NOT in the data you passed to genetic_isolation().")

  if( mode == "PGD" && is.null(axis) && is.null(forward) )
    stop("mode = \"PGD\" needs a direction: give 'axis' or 'forward'.")

  strata <- unique( as.character(x[[stratum]]) )

  # Positions along an axis, one per stratum
  a <- NULL
  if( !is.null(axis) ) {
    if( is.character(axis) && length(axis) == 1 ) {
      if( !(axis %in% names(x)) || !is.numeric(x[[axis]]) )
        stop("'axis' must name a numeric column of x, or be a numeric vector named by stratum.")
      a <- tapply( x[[axis]], as.character(x[[stratum]]), mean, na.rm = TRUE )
    } else {
      if( !is.numeric(axis) || is.null(names(axis)) || !all(strata %in% names(axis)) )
        stop("A numeric 'axis' vector must be named by stratum and cover every stratum.")
      a <- axis
    }
    a <- a[strata]
    sep_name <- if( is.character(axis) ) paste("separation along", axis) else "separation along axis"
  }

  # Separation among strata
  if( !is.null(spatial) ) {
    D <- as.matrix(spatial)
    if( is.null(rownames(D)) || !all(strata %in% rownames(D)) )
      stop("'spatial' needs row and column names covering every stratum.")
    D <- D[strata, strata]
    sep_name <- deparse1(substitute(spatial))
  } else if( !is.null(a) ) {
    D <- abs( outer(a, a, "-") )
  } else {
    if( !all(c("Longitude", "Latitude") %in% names(x)) )
      stop("Supply 'spatial' or 'axis', or give x 'Longitude' and 'Latitude' columns.")
    D <- strata_distance( strata_coordinates(x, stratum = stratum), mode = "Circle" )
    D <- D[strata, strata]
    sep_name <- "great-circle distance (km)"
  }

  if( mode == "IBD" ) {
    G <- as.matrix( genetic_distance(x, stratum = stratum, mode = genetic, ...) )
    if( is.null(rownames(G)) || !all(strata %in% rownames(G)) )
      stop(sprintf("genetic_distance(mode = \"%s\") is not a distance among strata.", genetic))
    G <- G[strata, strata]
    ok <- upper.tri(G) & is.finite(G) & is.finite(D)
    nperm <- as.integer(nperm)
    r_obs <- stats::cor(G[ok], D[ok])
    fit <- stats::lm(G[ok] ~ D[ok])
    res <- list(
      statistic   = c(r = r_obs),
      parameter   = c(pairs = sum(ok), nperm = nperm),
      p.value     = .mantel_p(G, D, ok, nperm),
      estimate    = c(R2 = r_obs^2, slope = unname(stats::coef(fit)[2])),
      null.value  = c(r = 0),
      alternative = "greater",
      method      = sprintf("Isolation by distance (%s genetic distance; Mantel permutation test)", genetic),
      data.name   = sprintf("%s by %s, and %s", dname, stratum, sep_name))
    class(res) <- "htest"
    return( res )
  }

  # Graph-based modes
  graph <- population_graph( x, stratum = stratum, ... )
  ibgd_mode <- if( mode == "IBGD" ) "cgd" else "pgd"
  if( is.null(spatial) && !is.null(a) ) {
    res <- ibgd( graph, x = a, mode = ibgd_mode, gamma = gamma, nperm = nperm )
  } else {
    Fw <- forward
    if( is.null(Fw) && !is.null(a) )
      Fw <- outer( a, a, function(i, j) j > i )
    res <- ibgd( graph, distance = D, forward = Fw, mode = ibgd_mode, gamma = gamma, nperm = nperm )
  }
  res$data.name <- sprintf("Population Graph of %s by %s, and %s", dname, stratum, sep_name)
  return( res )
}


# One-sided Mantel permutation p-value for the correlation between the
# 'ok' cells of matrices Y and X, permuting the rows and columns of X.
.mantel_p <- function( Y, X, ok, nperm ) {
  r_obs <- stats::cor(Y[ok], X[ok])
  n <- nrow(X)
  r_null <- vapply(seq_len(nperm), function(b) {
    idx <- sample.int(n)
    stats::cor(Y[ok], X[idx, idx][ok])
  }, numeric(1))
  (1 + sum(r_null >= r_obs)) / (1 + nperm)
}
