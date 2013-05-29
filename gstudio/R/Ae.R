#' Effective Allelic Diversity
#' 
#' This function returns the effective number of alleles in a sample.
#' @param x A set of \code{locus} objects
#' @return The effective number of alleles
#' @export
#' @examples
#' loci <- c( locus(1:2), locus(c(1,1)), locus(c(2,2)), locus(2:3) )
#' Ae(loci)
Ae <- function( x ){
  if( !is(x,"locus"))
    stop("The function Ae() only works with objects of type 'locus'")
  
  ret <- 1 / ( 1 - He(x) )
  return( ret )
}
