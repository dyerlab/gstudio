#' Shannon information index
#'
#' Shannon's information index of allelic diversity at a locus,
#'  \eqn{I = -\sum_i p_i \ln p_i}, using the natural logarithm as in GenAlEx
#'  (Peakall & Smouse 2006).  It is zero for a monomorphic locus and
#'  \eqn{\ln k} for \eqn{k} equally frequent alleles.
#' @param x A \code{locus} vector.
#' @return The Shannon index at that locus.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @noRd
#' @examples
#' loci <- c(locus(1:2), locus(c(1,1)), locus(c(2,2)))
#' Sh(loci)
Sh <- function( x ) {
  if( !is(x, "locus") )
    stop("Sh() takes a locus vector.")
  p <- frequencies(x)$Frequency
  p <- p[ p > 0 ]
  return( -sum( p * log(p) ) )
}
