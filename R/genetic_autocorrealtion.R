#' Spatial Autocorrelation Analysis
#'
#' Performs the spatial genetic autocorrelation analysis of Smouse & Peakall
#' (1999) given a physical distance matrix and a genetic distance matrix.
#' Returns a tidy data frame of bin-level correlations that can be plotted
#' directly with \code{ggplot2}.
#'
#' @param P A square physical distance matrix.
#' @param G A square genetic distance matrix (same dimensions as \code{P}).
#' @param bins A numeric vector of bin boundaries. Pairs whose physical
#'   distance falls in \eqn{[\code{bins[i]}, \code{bins[i+1]})} are placed in
#'   lag \eqn{i}.
#' @param perms Number of permutations used to estimate a p-value at each
#'   distance bin (default 0 = no permutation test).
#' @return A data frame with one row per lag and columns \code{From},
#'   \code{To}, \code{R} (autocorrelation coefficient), \code{N} (number of
#'   pairs in the bin), and \code{P} (permutation p-value; \code{NA} when
#'   \code{perms = 0}).
#' @references Smouse, P.E. & Peakall, R. (1999) Spatial autocorrelation
#'   analysis of individual multiallele and multilocus genetic structure.
#'   \emph{Heredity} 82, 561–573.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' \dontrun{
#' data(arapat)
#' P <- as.matrix(dist(arapat[, c("Longitude", "Latitude")]))
#' G <- genetic_distance(arapat, mode = "Euclidean")
#' bins <- c(0, 1, 2, 3, 5)
#' ac <- genetic_autocorrelation(P, G, bins)
#'
#' library(ggplot2)
#' ggplot(ac, aes(x = To, y = R)) +
#'   geom_hline(yintercept = 0, linetype = "dashed") +
#'   geom_line() +
#'   geom_point() +
#'   labs(x = "Distance (upper bin boundary)", y = "Autocorrelation (r)")
#' }
#' @seealso \code{\link{genetic_distance}}
genetic_autocorrelation <- function(P, G, bins, perms = 0) {
  if (missing(P) || missing(G) || missing(bins))
    stop("Need P, G, and binsize for autocorrelation.")
  if (any(dim(P) != dim(G)))
    stop("Need both P and G to have the same dimensionality for genetic_autocorrelation()")

  C <- dist2cov(G)
  ret <- data.frame(
    From = bins[1:(length(bins) - 1)],
    To   = bins[2:length(bins)],
    R    = NA_real_,
    N    = NA_real_,
    P    = NA_real_
  )

  if (perms > 0)
    R <- data.frame(Bin = NA_real_, R = NA_real_)

  for (lag in 1:nrow(ret)) {
    X <- P
    X[X <  ret$From[lag]] <- 0
    X[X >= ret$To[lag]]   <- 0
    X[X != 0]             <- 1
    diag(X) <- rowSums(X)
    dX   <- diag(X)
    dC   <- diag(C)
    rbot <- sum(dX * dC)
    rtop <- sum(C * X) - rbot
    r    <- rtop / rbot
    ret$R[lag] <- r
    ret$N[lag] <- sum(X[lower.tri(X)])

    if (perms > 0) {
      p <- rep(0, perms)
      for (i in 1:perms) {
        cP   <- permute_matrix(C)
        rbot <- sum(diag(cP) * dX)
        rtop <- sum(cP * X) - rbot
        p[i] <- rtop / rbot
      }
      ret$P[lag] <- sum(p >= r) / perms
      R <- rbind(R, data.frame(Bin = ret$To[lag], R = p))
    }
  }

  return(ret)
}
