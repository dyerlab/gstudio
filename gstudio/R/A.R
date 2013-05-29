#' Allelic Diversity
#' 
#' This function returns the number of unique, non-NA alleles, in a sample.
#' @param x A set of \code{locus} objects
#' @param min_freq The minimum frequency of allele to consider (default=0)
#' @return The number of alleles that have \code{min_freq} frequency.
#' @export
#' @examples
#' loci <- c( locus(1:2), locus(c(1,1)), locus(c(2,2)), locus(2:3) )
#' A(loci)
#' A(loci, min_freq=0.13)
A <- function(x, min_freq=0 ){
  if( !is(x,"locus"))
    stop("The function A() only works with objects of type 'locus'")
  f <- frequencies( x )
  ret <- dim(f[ f$Frequency>=min_freq,] )[1]
  return( ret )
}

