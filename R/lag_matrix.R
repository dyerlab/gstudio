#' Returns matrix only with values within a particular bin distance range
#' 
#' This function returns bin distances for spatial autocorrelation.
#' @param X The pairwise distance matrix.
#' @param lower The lower limit of the bin being requested
#' @param upper The upper limit of the bin being requested
#' @return A matrix with the count of objects in each bin category.
#' @export
#' @examples
#' D <- matrix(c(0, 2, 5, 2, 0, 3, 5, 3, 0), nrow = 3)
#' lag_matrix(D, lower = 1, upper = 4)
#' 
lag_matrix <- function( X, lower=NULL, upper=NULL ){
  
  if( !is.matrix(X) | nrow(X) != ncol(X) | any( X != t(X)) )
    stop("This function requires a square, symmetric distance matrix.")
  
  if( is.null(lower) | is.null(upper) )
    stop("Please specify the ")
  
  ret <- X
  ret[ret < lower ] <- 0
  ret[ret >= upper] <- 0
  ret[ ret != 0 ] <- 1
  diag(ret) <- 0
  diag(ret) <- rowSums(ret)
  return( ret )
}