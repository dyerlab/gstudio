#' Moran spectral randomization (native)
#'
#' @description
#' Generates null replicates of a node-level variable that preserve its spectral
#' structure on a spatial weighting matrix (Moran spectral randomization, the
#' \dQuote{singleton} algorithm of Wagner & Dray 2015).  The variable is expanded
#' on the Moran eigenvector maps (MEMs) of the doubly-centred weight matrix, and
#' each replicate gives every coefficient an independent random sign, so each
#' replicate keeps the observed mean, variance and spectral power.
#'
#' This is a native port of \code{adespatial::msr(x, listw, method = "singleton")}
#' with \code{spdep::mat2listw(W, style = "B")} weights, written so gstudio does not
#' depend on adespatial or spdep.  With the same random seed it consumes random
#' numbers in the same order as adespatial and returns the same replicates (up to
#' floating-point rounding).
#'
#' @param z Numeric vector, one value per node.
#' @param W Symmetric non-negative weight matrix (\eqn{n \times n}); asymmetric
#'   input is symmetrised as \eqn{(W + W^T)/2}.
#' @param nrepet Number of replicates.
#' @return An \eqn{n \times} \code{nrepet} matrix of null replicates.
#' @references Wagner HH, Dray S (2015) Generating spatially constrained null
#'   models for irregularly spaced data using Moran spectral randomization
#'   methods. \emph{Methods in Ecology and Evolution} \strong{6}: 1169--1178.
#' @seealso \code{\link{source_sink_test}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' W <- matrix(0, 6, 6); for (i in 1:5) W[i, i + 1] <- W[i + 1, i] <- 1
#' set.seed(1)
#' msr_null(c(3, 1, 4, 1, 5, 9), W, nrepet = 5)
#' @export
msr_null <- function(z, W, nrepet = 999L) {
  z <- as.numeric(z); W <- as.matrix(W); n <- length(z)
  if (!all(dim(W) == n)) stop("'W' must be n x n with n = length(z)")
  if (n < 3) stop("at least 3 nodes are needed")
  if (!isSymmetric(unname(W))) W <- (W + t(W)) / 2
  mem <- .mem_basis(W)
  r <- as.numeric(stats::cor(z, mem))
  zm <- mean(z); zs <- stats::sd(z); nv <- length(r)
  out <- matrix(NA_real_, n, nrepet)
  for (k in seq_len(nrepet)) {
    a <- vapply(r, function(ri) sample(c(-1, 1), 1) * ri, numeric(1))     # adespatial genbysingleton()
    out[, k] <- sqrt(n - 1) * as.numeric(mem %*% a) * zs + zm
  }
  out
}

#' Orthonormal Moran eigenvector basis (n - 1 vectors), as adespatial::scores.listw(MEM.autocor = "all")
#' @keywords internal
.mem_basis <- function(W) {
  n <- nrow(W); wt <- rep(1 / n, n)
  rm <- colSums(wt * W); cm <- colSums(wt * t(W)); cm <- cm - sum(rm * wt)      # ade4::bicenter.wt
  X <- sweep(W, 2, rm); X <- t(sweep(t(X), 2, cm))
  X <- X * sqrt(wt); X <- t(t(X) * sqrt(wt))
  e <- eigen(X, symmetric = TRUE)
  eq0 <- which(vapply(e$values / max(abs(e$values)), function(v) isTRUE(all.equal(v, 0)), logical(1)))
  if (!length(eq0)) stop("Illegal weight matrix: no null eigenvalue")
  if (length(eq0) == 1) {
    vec <- e$vectors[, -eq0, drop = FALSE]
  } else {
    q <- qr.Q(qr(cbind(wt, e$vectors[, eq0])))
    e$vectors[, eq0] <- q[, -ncol(q)]
    vec <- e$vectors[, -eq0[1], drop = FALSE]
  }
  vec / sqrt(wt) / sqrt(n)                                 # adespatial: / wtsqrt, then msr: / sqrt(n)
}
