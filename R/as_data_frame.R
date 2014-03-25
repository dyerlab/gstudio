

#' Converts locus to a data frame
#' 
#' This converts a \code{locus} object into a \code{data.frame} so that
#'  you can use \code{locus} objects in a column of a \code{data.frame}.
#' @param x An object of type \code{code}.  This can be either a single genotype 
#'  (a rare case) or a vector of genotypes (preferred). 
#' @param \dots Additional objects that are passed to \code{as.data.frame.vector}.
#' @return A \code{data.frame} object.
#' @note If you do not assign a data name to the \code{x} in assignment (e.g., 
#'  TPI=x ) it will name the column in the \code{data.frame} the same as the name
#'  of the variable you assigned it.  If this is confusing see the examples.
#' @method as.data.frame locus
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' 
#' loc1 <- c( locus(1:2), locus(1:2), locus( c(1,1) ) )
#' df <- data.frame( ID=1:3, NAMED_LOCUS=loc1, loc1 )
#' summary(df)
#' 
as.data.frame.locus <- function( x, ... ) {
  nm <- deparse( substitute(x), width.cutoff = 500L )
  return( as.data.frame.vector(x, ..., nm=nm ) ) 
}










