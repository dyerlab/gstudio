#' Estimate expected heterozygosity
#'  
#' Returns the general expected heterozygosity parameter
#'  from the frequencies
#' @param x Either a \code{data.frame} object with \code{locus} objects or 
#'  a vector or \code{locus} objects.
#' @param small.sample.correction Apply the 2N/(2N-1) correction to the data
#'  for small sample sizes.
#' @return The expected heterozygosity as a numeric or a \code{data.frame} if 
#'  several loci are passed.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' loci <- c( locus( c("A","A") ), locus( c("A","A") ), locus( c("A","B") ) )
#' He( loci )
#' He( loci, small.sample.correction=TRUE )
He <- function( x, small.sample.correction=FALSE ) { 
  
  if( is(x,"data.frame") ){
    locus_names <- column_class(x,class="locus")
    if( length(locus_names)==0)
      stop("Cannot estimate expected heterozygosity if there are no loci...")
    
    ret <- data.frame( Locus=locus_names, He=0 )
    k <- length(locus_names)
    for( i in 1:k)
      ret$He[i] <- He( x[[locus_names[i]]], small.sample.correction )
    return( ret )
  }
  
  else if( is( x, "locus")){
    df <- frequencies(x)
    he <- 1.0 - sum( df$Frequency**2 )
    Ninds <- sum( ploidy(x)>1 )
    
    if( small.sample.correction )
      he <- he * 2 * (Ninds)/( 2 * Ninds - 1 )
    
    return( he )    
  }
  
  else 
    stop("he() is only available for objects of type 'locus' or type 'data.frame'")

}
