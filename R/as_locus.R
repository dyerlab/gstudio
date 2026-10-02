#' As operator for locus
#' 
#' This takes several things and shoves it into the constructor
#' @param x An object that is to be turned into a \code{locus}.
#' @return An object of type \code{locus}
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @seealso \code{\link{locus}}
#' @examples
#' 
#' lst <- list( "A", "B" )
#' as.locus( lst )
#' vec <- 1:2
#' as.locus( lst )
#' chr <- "A"
#' as.locus( chr )
#' chr.sep <- "A:A"
#' as.locus( chr )
#' 
as.locus <- function( x ) {
  if( inherits(x,"list"))
    x <- unlist(x)
  return( locus(x) )
}
