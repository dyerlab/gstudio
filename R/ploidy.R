#' Returns the number of alleles in a \code{locus} object
#' 
#' This function will a count of the number of alelles within 
#' a \code{locus} object for either a single locus or for a 
#' vector of loci
#' @param x A \code{locus} object (single or vector)
#' @return A count of the number of alleles in the \code{locus}
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' loc <- locus( 1:2 )
#' ploidy( loc )
#' loci <- c( locus(1:2), locus(c(1,1) ) )
#' ploidy( loci )
ploidy <- function ( x ) {
  if( is(x,"data.frame")) {
    loci <- column_class(x,"locus")
    K <- length(loci)
    ret <- data.frame(Locus=loci,Ploidy=0)
    for( locus in loci )
      ret$Ploidy[ ret$Locus==locus] <- mean(ploidy( x[[locus]] ))
    return(ret)
  }
  else if( length(x)>1 )
    return( unlist(lapply(x,ploidy) ) )
  else
    return( length( alleles( x ) ) )
}

