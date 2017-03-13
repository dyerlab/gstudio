#' Converts distance matrix to covarance one.
#' 
#' This function takes a distance matrix and turns it into a covariance
#'   matrix following Gower (1966).
#' @param D A pairwise, square, and symmetric distance matrix.   
#' @return A covariance matrix of the same size.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
dist2cov <- function( D ) {
  if( dim(D)[1] != dim(D)[2] )
    stop("Cannot use non-symmetric matrices for this...")
  K <- dim(D)[1]
  
  D1 <- matrix( rowSums(D), ncol=K, nrow=K)
  D2 <- matrix( rowSums(t(D)), ncol=K, nrow=K, byrow=TRUE)
  
  C <- -1*D + (D1 + D2)/K - sum(D)/K^2
  return( C/2.0 )
}


