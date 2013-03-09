#' Covariance Conversion
#' 
#' Convert the matrix of covariance values into pair-wise distance
#'  values.
#' @param x A square \code{matrix} representing covariance
#' @return A square matrix of pair-wise distance values.
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @seealso \code{\link{dist_2_cov}}
#' 
cov_2_dist <- function( x ) {
  if( !is.matrix(x) )
    stop("This function only works on matrices")
  else if( dim(x)[1] != dim(x)[2]) 
    stop("This function only works on square matrices")
  
  N <- dim(x)[1]
  D <- matrix(0,N,N)
  
  for(i in 1:N){
    for( j in i:N){
      D[i,j] <- D[j,i] <- x[i,i] + x[j,j] - 2*x[i,j]
    }
  }
  
  return(D)
}

