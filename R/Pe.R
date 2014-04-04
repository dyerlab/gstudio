#' Polymorphic index for loci
#' 
#' This function returns the effective number of alleles in a sample.
#' @param x A set of \code{locus} objects either as a vector or within a 
#'  \code{data.frame}.
#' @return The polymorphic index for the locus, sum(pi(1-pi)) as a \code{numeric} value
#'  or as a \code{data.frame} if mutliple loci are passed.
#' @export
#' @examples
#' locus <- c( locus(1:2), locus(c(1,1)), locus(c(2,2)), locus(2:3) )
#' Pe(locus)
#' locus2 <- c( locus(1:2), locus(c(1,1)), locus(c(2,2)), locus(2:3) )
#' df <- data.frame( locus, locus2 )
#' Pe(df)
Pe <- function( x  ) {
  
  if( is( x, "data.frame" ) ){
    locus_names <- column_class(x,"locus")
    if( length(locus_names)==0 )
      stop("You must pass some loci to the Ae() function.")
    K <- length(locus_names)
    ret <- data.frame( Locus=locus_names, Ae=0 )
    for( i in 1:K){
      data <- x[[locus_names[i]]]
      ret[i,2] <- Pe( data )
    }  
  }
  
  else if( is(x,"locus") ) {
    f <- frequencies( x )
    ret <- sum(f$Frequency * (1-f$Frequency) )
  }
    
  
  else
    stop("The function Pe() only works with objects of type 'locus' or data.frame objects containing 'locus' columns")
  
  return( ret )
}
