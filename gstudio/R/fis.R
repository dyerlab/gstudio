#' Estimate simple inbreeding from frequencies
#' 
#' Returns the general Fis = 1-ho/he parameter from the locus being passed or
#'  a set of them if the value passed is a \code{data.frame} with locus objects.
#'  
#' @param x Either a \code{locus} object or a data.frame with locus objects.
#' @return The inbreeding F statistic
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' 
#' loci <- c( locus( c("A","A") ), locus( c("A","A") ), locus( c("A","B") ) )
#' Fis( loci )
#' 
Fis <- function( x ) {
  if( is(x,"locus") )
    return( 1.0 - Ho(x) / He(x) )
  else if( is(x,"data.frame")) {
    cols <- column_class( x, "locus" )
    ret <- list()
    for( col in cols )
      ret[col] <- Fis( x[[col]] )
    ret <- as.numeric( ret )
    names(ret) <- cols
    return( ret )
  }
  else
    stop(paste("This function does not know how to handle data of type",class(x)))
  
}
