#' Determines if \code{locus} is NA
#' 
#' Convienence function to determine if the \code{locus} object is NA
#' @param x The \code{locus} object
#' @return A logical flag indicating if \code{x} is NA
#' @method is.na locus
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
is.na.locus <- function( x ) {
  
  func <- function(x) {
    return ( length(alleles(x))==0 ||
               any( is.na(alleles(x))))
  }
  
  return ( unlist(lapply( x, func) ) )
}
