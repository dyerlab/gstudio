#' Estimation of Bray-Curtis distance
#' 
#' This function returns a measure of genetic distance based upon
#'  the Cavalli Sforza distance metric.  
#' @param idx indices to compare
#' @param data The genotypes to examine.
#' @return The Cavalli-Sforza Genetic distance
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
dist_cavalli <- function( idx, data ) {
  px <- data[idx[1],]
  py <- data[idx[2],]
  ret <-  2/pi * sqrt( 2 * ( 1- sum( sqrt( px*py  ) ) ) )
  return(ret)
}
