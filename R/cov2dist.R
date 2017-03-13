#' Converts covariance matrix to distance one.
#' 
#' This function takes a covariance matrix and turns it into a distance
#'   matrix following Gower (1966).
#' @param C A pairwise, square, and symmetric distance matrix.   
#' @return A distance matrix of the same size.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
cov2dist <- function( C ) {
  if( dim(C)[1] != dim(C)[2] )
    stop("Cannot use non-symmetric matrices for this...")
  K <- dim(C)[1]

  D <- matrix(0,nrow=K,ncol=K)
  for( i in 1:K){
    for( j in (i+1):K) {
      if( j <= K )
        D[i,j] <- C[i,i] + C[j,j] - 2.0*C[i,j]
    }
  }
  return( D + t(D) )
}


