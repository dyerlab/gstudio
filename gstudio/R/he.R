#' Estimate expected heterozygosity
#'  
#' Returns the general expected heterozygosity parameter
#'  from the frequencies
#' @param x A \code{data.frame} object from \code{frequencies}
#' @param small.sample.correction Apply the 2N/(2N-1) correction to the data
#'  for small sample sizes.
#' @return The expected heterozygosity
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' 
#' loci <- c( locus( c("A","A") ), locus( c("A","A") ), locus( c("A","B") ) )
#' He( loci )
#' He( loci, small.sample.correction=TRUE )
#' 
He <- function( x, small.sample.correction=FALSE ) { 
  if( !is(x,"locus") )
    stop("he() is only available for objects of type 'locus'")
  df <- frequencies(x)
  he <- 1.0 - sum( df$Frequency**2 )
  Ninds <- sum( ploidy(x)>1 )
  
  if( small.sample.correction )
    he <- he * 2 * (Ninds)/( 2 * Ninds - 1 )
  
  return( he )
}
