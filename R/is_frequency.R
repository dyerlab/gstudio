#' Is-A function for Allele Frequences
#' 
#' This is a function that returns a flag indicating that the 
#'  object passed has the qualities of a \code{data.frame} created
#'  by the function \code{frequencies}.
#' @param x A \code{data.frame} potentially from \code{frequencies}
#'  function.
#' @return A flag indicating it has the qualities of a \code{data.frame}
#'  representing allele frequencies
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
is_frequency <- function( x ) {
  return( is(x,"data.frame") & ("Allele" %in% names(x)) & ("Frequency" %in% names(x) ))
}
