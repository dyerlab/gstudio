#' Estimation Nei's Gst parameter with Hedrick's correction for allelic diversity
#' 
#' This function estimates the parameter (and potentially the confidence
#'  surrounding its value) for Gst.  It is corrected by the diversity of the 
#'  parameter as outlined by Hedrick.
#' @param x  A vector of \code{\link{locus}} objects or a \code{data.frame} with \code{locus} objects.
#' @param stratum Either a vector of strata variables if \code{x} is a \code{locus} vector or 
#'  the name of the column representing strata in \code{x} if it is a \code{data.frame}.
#' @param nperm The number of permutations to run for significance of the
#'  estimator.
#' @param size.correct A flag indicating that the estimate should be corrected for
#'  based upon sample sizes (default=TRUE).
#' @return A \code{data.frame} with Gst_prime, Hs, Ht and P.  If more than one locus is provided,
#'  then a 'mutlilocus' estimate is shown using the harmonic mean of individual locus Gst_prime values (monomorphic loci and negative estimates excluded).
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @noRd
#' @examples
#'  a1 <- sample( LETTERS[1:5], size=20, replace=TRUE)
#'  a2 <- sample( LETTERS[4:8], size=20, replace=TRUE)
#'  raw_alleles <- matrix( c(a1,a2), ncol=2, byrow=TRUE )
#'  locus <- locus( raw_alleles, type="column")
#'  Population <- c(rep("Pop-A",10),rep("Pop-B",10))
#'  Gst_prime( locus, Population )
#'  a1 <- sample( LETTERS[1:5], size=20, replace=TRUE)
#'  a2 <- sample( LETTERS[4:8], size=20, replace=TRUE)
#'  raw_alleles <- matrix( c(a1,a2), ncol=2, byrow=TRUE )
#'  locus2 <- locus( raw_alleles, type="column")
#'  df <- data.frame( Population, TPI=locus, PGM=locus2 )
#'  Gst_prime( df, nperm=99)
Gst_prime <- function(  x, stratum="Population",  nperm=0, size.correct=TRUE ) {

  if( is(x,"data.frame") ) {
    stratum <- .detect_stratum(x, stratum, default = "Population")
    return( .structure_table( "gst_prime", "Gst_prime", x, stratum, nperm, size.correct ) )
  }

  if( !is(x,"locus") )
    stop("This function requires objects of type 'locus' to function.")
  if( missing( stratum) )
    stop("You need to specify strata for Gst_prime to be estimated.")
  if( length(x) != length(stratum))
    stop("If you pass vectors to Gst_prime() for loci and stratum, they must be the same size...")

  r <- .structure_single( "gst_prime", x, stratum, nperm, size.correct )
  return( data.frame( Gst_prime=r$est, Hs=r$hs, Ht=r$ht, P=r$P ) )
}
