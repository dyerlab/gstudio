#' Takes distance matrix and turns it into a covariance matrix
#' 
#' This function takes a distance matrix and turns it into a 
#'   covariance matrix.
#' @param X A square and symmetric distance matrix.
#' @return A covariance matrix the same size as \code{X}
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples 
#' D <- matrix( c(0,7,6,7,7,0,13,8,6,13,0,4,7,8,4,0), nrow=4)
#' C <- distance_to_covariance(D)
#' C
distance_to_covariance <- function( X ) {
  if( !is(X,"matrix") | (nrow(X) != ncol(X)) | any( X != t(X) ))
    stop("You must pass a square, symmetric distance matrix to this function")
  r <- nrow(X)
  c <- ncol(X)
  
  Xi <- matrix( colSums(X), nrow=r, ncol=c, byrow = TRUE  )
  Xj <- matrix( rowSums(X), nrow=r, ncol=c )
  X2 <- X^2
  
  C = (-1*X + (Xi + Xj)/r - X2/r^2 )/2
  return( C )
}