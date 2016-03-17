#' Allelic Diversity
#' 
#' This function returns the number of unique, non-NA alleles, in a sample.
#' @param x A set of \code{locus} objects
#' @param min_freq The minimum frequency of allele to consider (default=0)
#' @param loci A list of loci to use (if a \code{data.frame}), if not given
#'  then all will be considered.
#' @param ... Ignored
#' @return The number of alleles that have \code{min_freq} frequency.
#' @export
#' @examples
#' loci <- c( locus(1:2), locus(c(1,1)), locus(c(2,2)), locus(2:3) )
#' A(loci)
#' A(loci, min_freq=0.13)
A <- function(x, min_freq=0, loci = NULL, ...){

  if( is( x, "data.frame" ) ){
    if( is.null(loci))
      locus_names <- column_class(x,"locus")
    else
      locus_names <- loci

    if( length(locus_names)==0 || !all(locus_names %in% names(x)) )
      stop("You must pass some data that has locus object to the A() function.")
    K <- length(locus_names)
    ret <- data.frame( Locus=locus_names, A=0 )
    for( i in 1:K){
      data <- x[[locus_names[i]]]
      ret[i,2] <- A( data, min_freq=min_freq )
    }  
    if( min_freq > 0 ){
      idx <- which( names(ret) == "A")
      names(ret)[idx] <- "A95"
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

