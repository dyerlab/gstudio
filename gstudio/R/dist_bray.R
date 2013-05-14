#' Estimation of Bray-Curtis distance
#' 
#' This function returns a measure of genetic distance based upon
#'  the Bray-Curtis distance metric.  
#' @param idx indices to compare
#' @param data The genotypes to examine.
#' @param nloc The number of loci 
#' @return The Bray-Curtis set dissimilarity
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
dist_bray <- function( idx, data, nloc=1 ) {
  x <- cbind( data[idx[1],], data[idx[2],])
  ret <- -log( sum(apply(x,1,min)) / nloc )
  return(ret)
}



