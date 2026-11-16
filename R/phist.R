#' Estimation of PhiST by Analysis of Molecular Variance
#'
#' Partitions the squared AMOVA distances among individuals (Smouse & Peakall
#'  1999; see \code{genetic_distance(mode = "AMOVA")}) into components among and
#'  within strata (Excoffier, Smouse & Quattro 1992) and returns
#'  PhiST = sigma2_among / (sigma2_among + sigma2_within).
#' @param x A \code{data.frame} with \code{locus} columns and a stratum column.
#' @param stratum The name of the stratum column (default "Population").
#' @param nperm The number of permutations used to test PhiST = 0 (default 0).
#' @return A \code{data.frame} with Locus, PhiST, sigma2_among and sigma2_within
#'  (loci in column order) and, when there is more than one locus, a
#'  \code{Multilocus} row.  Each locus uses only the individuals genotyped at it.
#'  The Multilocus components are the sums of the per-locus components, which with
#'  complete data equals an AMOVA of the distances summed across loci.  When
#'  \code{nperm > 0} it also has \code{P}, the right-tailed add-one permutation
#'  p-value, with strata permuted across individuals (the same permutation for
#'  every locus).
#' @references
#'   Excoffier L., Smouse P.E. & Quattro J.M. (1992) Analysis of molecular
#'   variance inferred from metric distances among DNA haplotypes. Genetics 131:
#'   479-491.
#'
#'   Smouse P.E. & Peakall R. (1999) Spatial autocorrelation analysis of
#'   individual multiallele and multilocus genetic structure. Heredity 82:
#'   561-573.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @noRd
PhiST <- function( x, stratum = "Population", nperm = 0 ) {

  if( !is( x, "data.frame" ) )
    stop("This function requires you to pass it a data.frame of data...")

  loci <- column_class( x, "locus" )
  if( length( loci ) == 0 || !all( loci %in% names( x ) ) )
    stop("You need to pass this function a data.frame with some columns of type 'locus' to it...")

  if( !( stratum %in% names( x ) ) )
    stop("You need to pass this function the name of the column to use as a stratum")

  strata <- as.character( x[[ stratum ]] )

  # One squared-distance matrix per locus, on the individuals typed at that locus
  # (and with a known stratum).
  keep <- lapply( loci, function( l ) which( !is.na( x[[ l ]] ) & !is.na( strata ) ) )
  D <- lapply( seq_along( loci ), function( i ) {
    dist_amova( x[[ loci[i] ]][ keep[[i]] ] )
  })

  # Sigma-squared components for one locus given the stratum labels of its rows.
  components <- function( Dl, g ) {
    N <- length( g )
    n <- as.numeric( table( g ) )
    K <- length( n )
    if( N < 2 || K < 2 || N <= K )
      return( c( NA_real_, NA_real_ ) )
    SS_total <- sum( Dl ) / ( 2 * N )
    # Within-stratum sums of D in O(N^2), regardless of the number of strata.
    block <- rowsum( t( rowsum( Dl, g, reorder = TRUE ) ), g, reorder = TRUE )
    SS_within <- sum( diag( block ) / ( 2 * n ) )
    MS_among <- ( SS_total - SS_within ) / ( K - 1 )
    MS_within <- SS_within / ( N - K )
    n0 <- ( N - sum( n^2 ) / N ) / ( K - 1 )
    c( ( MS_among - MS_within ) / n0, MS_within )
  }

  estimate <- function( labels ) {
    comp <- t( vapply( seq_along( loci ), function( i ) {
      components( D[[i]], labels[ keep[[i]] ] )
    }, numeric( 2 ) ) )
    if( length( loci ) > 1 ) {
      ok <- stats::complete.cases( comp )
      multi <- if( any( ok ) ) colSums( comp[ ok, , drop = FALSE ] ) else c( NA_real_, NA_real_ )
      comp <- rbind( comp, multi )
    }
    phi <- comp[ , 1 ] / ( comp[ , 1 ] + comp[ , 2 ] )
    phi[ !is.finite( phi ) ] <- NA_real_
    list( phi = phi, comp = comp )
  }

  obs <- estimate( strata )
  ret <- data.frame( Locus = if( length( loci ) > 1 ) c( loci, "Multilocus" ) else loci,
                     PhiST = obs$phi,
                     sigma2_among = obs$comp[ , 1 ],
                     sigma2_within = obs$comp[ , 2 ],
                     stringsAsFactors = FALSE )
  rownames( ret ) <- NULL

  if( nperm > 0 ) {
    count <- numeric( nrow( ret ) )
    used  <- numeric( nrow( ret ) )
    message("permuting ", appendLF = FALSE)
    for( rep in seq_len( nperm ) ) {
      if( rep %% 10 == 0 )
        message(".", appendLF = FALSE)
      null <- estimate( sample( strata ) )$phi
      ok <- !is.na( null ) & !is.na( ret$PhiST )
      count <- count + ( ok & null >= ret$PhiST )
      used  <- used + ok
    }
    message("")
    # Add-one permutation p-value (Phipson & Smyth 2010), as for the other modes.
    ret$P <- ifelse( is.na( ret$PhiST ), NA_real_, ( 1 + count ) / ( 1 + used ) )
  }

  return( ret )
}
