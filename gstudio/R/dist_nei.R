#' Estimation of Bray-Curtis distance
#' 
#' This function returns a measure of genetic distance based upon
#'  the Nei's Genetic distance metric.  
#' @param idx indices to compare
#' @param data The genotypes to examine.
#' @return The Nei Genetic distance
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
dist.nei <- function( idx, data ) {
  p1 <- data[idx[1],]
  p2 <- data[idx[2],]
  
  top <- sum( p1*p2 )
  bot <- sqrt( sum( p1^2) * sum(p2^2) )
  
  ret <- -1*log(top/bot) 
  return(ret)
}
