#' Estimate simple inbreeding from frequencies
#' 
#' Returns the general Fis = 1-ho/he parameter
#'  from the frequencies
#' @param x A \code{data.frame} from \code{frequencies} for a \code{locus}
#' @return The inbreeding F statistic
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' 
#' loci <- c( locus( c("A","A") ), locus( c("A","A") ), locus( c("A","B") ) )
#' Fis( loci )
#' 
Fis <- function( x ) {
  return( 1.0 - Ho(x) / He(x) )
}
