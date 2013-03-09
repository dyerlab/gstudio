#' Estimation of amova distance
#' 
#' This function returns a measure of genetic distance based upon
#'  the AMOVA distance metric.  
#' @param idx indices to compare
#' @param data The genotypes to examine.
#' @return The AMOVA distance between two individuals.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
dist_amova <- function( idx, data ) {
  x <- data[idx[1],] 
  y <- data[idx[2],]
  ret <- sum( 2*t(x-y) %*% (x-y) )
  return( ret ) 
}
