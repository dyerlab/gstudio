#' Paternity exclusion probability
#'
#' The probability of excluding a random, unrelated male as the father when
#'  the mother's genotype is known (Jamieson & Taylor 1997):
#'  \deqn{P = \sum_i p_i (1-p_i)^2 - \frac{1}{2} \sum_{i \ne j} p_i^2 p_j^2 (4 - 3p_i - 3p_j)}
#' @param x A \code{locus} vector.
#' @return The exclusion probability at that locus.  Across loci, combine as
#'  \eqn{1 - \prod_\ell (1 - P_\ell)}, as \code{genetic_diversity(mode = "Pex")}
#'  does for its \code{Multilocus} row.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @noRd
#' @examples
#' loci <- c(locus(1:2), locus(c(1,1)), locus(c(2,2)))
#' Pex(loci)
Pex <- function( x ) {
  if( !is(x, "locus") )
    stop("Pex() takes a locus vector.")
  p <- frequencies(x)$Frequency
  first <- sum( p * (1 - p)^2 )
  pp <- outer(p^2, p^2) * (4 - 3 * outer(p, p, "+"))
  diag(pp) <- 0
  return( first - 0.5 * sum(pp) )
}
