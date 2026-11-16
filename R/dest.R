#' Estimation of Jost's D_est 
#' 
#' This function estimates the parameter (and potentially the confidence
#'  surrounding its value) for Jost's (2008) D_est, [(Ht - Hs)/(1 - Hs)] * [k/(k - 1)].
#' @param x  A vector of \code{\link{locus}} objects or a \code{data.frame} with \code{locus} objects.
#' @param stratum Either a vector of strata variables if \code{x} is a \code{locus} vector or 
#'  the name of the column representing strata in \code{x} if it is a \code{data.frame}.
#' @param nperm The number of permutations to run for significance of the
#'  estimator.
#' @param size.correct A flag indicating that the estimate should be corrected for
#'  based upon sample sizes (default=FALSE).
#' @param multilocus How the multilocus estimate is formed: \code{"harmonic"} (harmonic
#'  mean of per-locus estimates, monomorphic loci and negative estimates excluded) or
#'  \code{"average"} (the estimator applied to the across-locus means of Hs and Ht).
#'  Only used for a \code{data.frame} with more than one locus.
#' @return A \code{data.frame} with Dest, Hs, Ht and P.  If more than one locus is
#'  provided, a \code{Multilocus} row is added.  Estimators, assumptions and
#'  differences from mmod and hierfstat are documented in \code{?genetic_structure}.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @noRd
#' @examples
#'  a1 <- sample( LETTERS[1:5], size=20, replace=TRUE)
#'  a2 <- sample( LETTERS[4:8], size=20, replace=TRUE)
#'  raw_alleles <- matrix( c(a1,a2), ncol=2, byrow=TRUE )
#'  locus <- locus( raw_alleles, type="column")
#'  Population <- c(rep("Pop-A",10),rep("Pop-B",10))
#'  Dest( locus, Population )
#'  a1 <- sample( LETTERS[1:5], size=20, replace=TRUE)
#'  a2 <- sample( LETTERS[4:8], size=20, replace=TRUE)
#'  raw_alleles <- matrix( c(a1,a2), ncol=2, byrow=TRUE )
#'  locus2 <- locus( raw_alleles, type="column")
#'  df <- data.frame( Population, TPI=locus, PGM=locus2 )
#'  Dest( df, nperm=99)
Dest <- function( x, stratum="Population", nperm=0, size.correct=FALSE, multilocus=c("harmonic","average") ) {

  if( is(x,"data.frame") ) {
    stratum <- .detect_stratum(x, stratum, default = "Population")
    if( !(stratum %in% names(x)) )
      stop("If you pass a data.frame to Dest(), you need to indicate a stratum variable column.")
    return( .structure_table( "dest", "Dest", x, stratum, nperm, size.correct, multilocus ) )
  }

  if( !inherits(x,"locus") )
    stop("This function requires objects of type 'locus' to function.")
  if( length(stratum) != length(x))
    stop("If you are going to pass vector loci and strata, they should be the same size...")

  r <- .structure_single( "dest", x, stratum, nperm, size.correct )
  ret <- data.frame( Dest=r$est, Hs=r$hs, Ht=r$ht )
  if( nperm > 0 )
    ret$P <- r$P
  return( ret )
}
