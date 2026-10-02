#' An 'is-a' operator for \code{locus}
#' 
#' A quick convienence function to determine if an object is
#'  inherited from the \code{locus} object.
#' @param x An object to query
#' @return A logical flag indicating if \code{x} is a type of \code{locus}
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' 
#' loc1 <- locus( c("A","A") )
#' is.locus( loc1 )
#' is.locus( FALSE )
#' is.locus( 23 )
#' 
is.locus <- function ( x ) { 
  return( inherits(x,"locus"))
}
