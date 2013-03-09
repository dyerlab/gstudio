#' Estimation of jaccard distance
#' 
#' This function returns a measure of genetic distance based upon
#'  the AMOVA distance metric.  Jaccard distance is essentially
#'  2b/(1+b) where b is Bray-Curtis distnance.  
#' @param idx indices to compare
#' @param data The genotypes to examine.
#' @param nLoc The number of loci (default=1)
#' @return The Jaccard set dissimilarity
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
dist_jaccard <- function( idx, data, nLoc=1 ) {
  b  <- dist_bray( idx, data, nLoc)
  return( (2*b)/(1+b))
}
