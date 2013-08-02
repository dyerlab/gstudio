#' Effective Allelic Diversity
#' 
#' This function returns the effective number of alleles in a sample.
#' @param x A set of \code{locus} objects either as a vector or within a 
#'  \code{data.frame}.
#' @return The effective number of alleles either as a \code{numeric} value
#'  or as a \code{data.frame} if mutliple loci are passed.
#' @export
#' @examples
#' locus <- c( locus(1:2), locus(c(1,1)), locus(c(2,2)), locus(2:3) )
#' Ae(locus)
#' locus2 <- c( locus(1:2), locus(c(1,1)), locus(c(2,2)), locus(2:3) )
#' df <- data.frame( locus, locus2 )
#' Ae(df)
Ae <- function( x  ) {
  
  if( is( x, "data.frame" ) ){
    locus_names <- column_class(x,"locus")
    if( length(locus_names)==0 )
      stop("You must pass some loci to the Ae() function.")
    K <- length(locus_names)
    ret <- data.frame( Locus=locus_names, Ae=0 )
    for( i in 1:K){
      data <- x[[locus_names[i]]]
      ret[i,2] <- Ae( data )
    }  
  }
  
  else if( is(x,"locus") ) 
    ret <- data.frame( Ae = 1 / ( 1 - He(x) ))
  
  else
    stop("The function Ae() only works with objects of type 'locus'")
  
  return( ret )
}
