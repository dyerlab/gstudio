#' Estimation Nei's Gst parameter
#' 
#' This function estimates Nei's Gst parameter and potentially
#'  returns the components of it as well as the probability.  The results are returned
#'  as a \code{data.frame}.
#' @param x  A vector of \code{\link{locus}} objects or a \code{data.frame} with \code{locus} objects.
#' @param stratum Either a vector of strata variables if \code{x} is a \code{locus} vector or 
#'  the name of the column representing strata in \code{x} if it is a \code{data.frame}.
#' @param nperm The number of permutations to run for significance of the
#'  estimator.
#' @param size.correct A flag indicating that the estimate should be corrected for
#'  based upon sample sizes (default=TRUE).
#' @return An \code{data.frame} with Gst, Ht, and Hs and optionally P
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @noRd
#' @examples
#'  AA <- locus( c("A","A") )
#'  AB <- locus( c("A","B") )
#'  BB <- locus( c("B","B") )
#'  locus <- c(AA,AA,AA,AA,BB,BB,BB,AB,AB,AA)
#'  Population <- c(rep("Pop-A",5),rep("Pop-B",5))
#'  Gst( locus, Population, nperm=99 )
#'  locus2 <- c(AB,BB,AA,BB,BB,AB,AB,AA,AA,BB)
#'  df <- data.frame( Population, TPI=locus, PGM=locus2 )
#'  Gst( df, nperm=99)
Gst <- function( x, stratum="Population", nperm=0, size.correct=TRUE ) {

  if( is(x,"data.frame") ) {
    stratum <- .detect_stratum(x, stratum, default = "Population")
    return( .structure_table( "gst", "Gst", x, stratum, nperm, size.correct ) )
  }

  if( !is( x, "locus") )
    stop(paste("This function requires objects of type 'locus' to function, you passed a '", class(x), "' object",sep="" ))
  if( length(x) != length(stratum) )
    stop("You must pass loci and strata vectors of the same length to Gst().")

  r <- .structure_single( "gst", x, stratum, nperm, size.correct )
  ret <- data.frame( Gst=r$est, Hs=r$hs, Ht=r$ht )
  if( nperm > 0 && !is.na(r$P) )
    ret$P <- r$P
  return( ret )
}
