#' Distance conversion
#' 
#' Conversion of pair-wise distances into covaraince estimates.
#' @param x A square \code{matrix} of distance values.
#' @return A square matrix representing covariance
#' @seealso \code{\link{cov_2_dist}}
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' 
dist_2_cov <- function( x ) {
  
  if( !is.matrix(x) )
    stop("This function requires a matrix to operate upon.")
  
  else if( dim(x)[1] != dim(x)[2] )
    stop("This function requires a square matrix.")
  
  tot <- mean(x)
  N <- dim(x)[1]
  colMu <- matrix( colMeans(x), nrow=N, ncol=N, byrow=T)
  
  C <- -0.5 * ( x - colMu - t(colMu) + tot )
  
  return(C)
}




