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

  if( is( x, "data.frame" ) ){
    locus_names <- column_class(x,"locus")
    if( length(locus_names)==0 )
      stop("You must pass some loci to the A() function.")
    K <- length(locus_names)
    ret <- data.frame( Locus=locus_names, A=0 )
    for( i in 1:K){
      data <- x[[locus_names[i]]]
      ret[i,2] <- A( data )
    }  
  }
  
  else if( is(x,"locus") ) {
    f <- frequencies( x )
    ret <- dim(f[ f$Frequency>=min_freq,] )[1]
  }
  
  else
    stop("The function Ae() only works with objects of type 'locus'")
  
  return( ret )
}

