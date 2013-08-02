#' Estimate simple inbreeding from frequencies
#' 
#' Returns the general Fis = 1-ho/he parameter from the locus being passed or
#'  a set of them if the value passed is a \code{data.frame} with locus objects.
#'  
#' @param x Either a \code{locus} object or a data.frame with locus objects.
#' @param small.sample.correction Passes this along to He for small sample sizes.
#' @return The inbreeding F statistic as a \code{numeric} value or a \code{data.frame}
#'  if you passed multiple loci to this function.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' loci <- c( locus( c("A","A") ), locus( c("A","A") ), locus( c("A","B") ) )
#' Fis( loci )
Fis <- function( x, small.sample.correction=FALSE ) {
  if( is(x,"locus") ) {
    ret <- 1.0 - Ho(x) / He(x, small.sample.correction)
    names(ret) <- "Fis"
  }
  else if( is(x,"data.frame")) {
    cols <- column_class( x, "locus" )
    ret <- data.frame( Locus=cols, Fis=0)
    for( i in 1:length(cols) )
      ret$Fis[i] <- Fis( x[[cols[i]]] )
  }
  else
    stop(paste("This function does not know how to handle data of type",class(x)))
  
  return( ret )
}
