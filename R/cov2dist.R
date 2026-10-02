#' Converts covariance matrix to distance one.
#' 
#' This function takes a covariance matrix and turns it into a distance
#'   matrix following Gower (1966).
#' @param C A pairwise, square, and symmetric distance matrix.   
#' @return A distance matrix of the same size.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' C <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
#' cov2dist(C)
cov2dist <- function( C ) {
  if( dim(C)[1] != dim(C)[2] )
    stop("Cannot use non-symmetric matrices for this...")
  d <- diag(C)
  D <- outer(d, d, "+") - 2.0 * C
  diag(D) <- 0
  return( D )
}


