#' Estimation of PhiST by Analysis of Molecular Variance
#'
#' Partitions the squared AMOVA distances among individuals (Smouse & Peakall
#'  1999; see \code{genetic_distance(mode = "AMOVA")}) into components among and
#'  within strata (Excoffier, Smouse & Quattro 1992) and returns
#'  PhiST = sigma2_among / (sigma2_among + sigma2_within).
#' @param x A \code{data.frame} with \code{locus} columns and a stratum column.
#' @param stratum The name of the stratum column (default "Population").
#' @param nperm The number of permutations used to test PhiST = 0 (default 0).
#' @param pairwise If \code{TRUE}, return a symmetric matrix of the multilocus
#'  PhiST (single-locus if only one locus) between every pair of strata.
#' @return An \code{amova} object (see \code{?amova}): a \code{data.frame} with
#'  Locus, PhiST, sigma2_among, sigma2_within and p.value (loci in column order) and, when there is more than one locus, a
#'  \code{Multilocus} row.  Individuals with a missing stratum are dropped, and
#'  each locus uses only the individuals genotyped at it.  The Multilocus
#'  components are the sums of the per-locus components, which with complete data
#'  equals an AMOVA of the distances summed across loci.  When \code{nperm > 0},
#'  p.value is the right-tailed add-one permutation p-value, with strata
#'  permuted across individuals (the same permutation for every locus).
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
PhiST <- function( x, stratum = "Population", nperm = 0, pairwise = FALSE ) {
  prep <- .amova_prep( x, stratum )
  if( pairwise )
    return( .amova_pairwise( prep ) )
  .amova_table( prep, nperm = nperm, name = "PhiST", stratum = stratum )
}

# Per-locus squared-distance matrices for an AMOVA.  Rows with a missing stratum
# are dropped; each locus keeps the rows genotyped at it.  With pollen_pool, the
# rows are paternal gametes (see dist_amova()).
#' @keywords internal
#' @noRd
.amova_prep <- function( x, stratum, pollen_pool = FALSE ) {

  if( !is( x, "data.frame" ) )
    stop("This function requires you to pass it a data.frame of data...")

  loci <- column_class( x, "locus" )
  if( length( loci ) == 0 || !all( loci %in% names( x ) ) )
    stop("You need to pass this function a data.frame with some columns of type 'locus' to it...")

  if( !( stratum %in% names( x ) ) )
    stop("You need to pass this function the name of the column to use as a stratum")

  strata <- as.character( x[[ stratum ]] )
  if( any( is.na( strata ) ) ) {
    message( sum( is.na( strata ) ), " individuals with a missing '", stratum, "' were dropped." )
    x <- x[ !is.na( strata ), , drop = FALSE ]
    strata <- strata[ !is.na( strata ) ]
  }

  keep <- lapply( loci, function( l ) which( !is.na( x[[ l ]] ) ) )
  D <- lapply( seq_along( loci ), function( i ) {
    dist_amova( x[[ loci[i] ]][ keep[[i]] ], pollen_pool = pollen_pool )
  })
  list( loci = loci, strata = strata, keep = keep, D = D )
}

# AMOVA quantities for one locus given row labels: sample sizes, degrees of
# freedom, sums of squares, mean squares and the sigma-squared components.
#' @keywords internal
#' @noRd
.amova_components <- function( Dl, g ) {
  out <- c( N = NA_real_, K = NA_real_, df_among = NA_real_, df_within = NA_real_,
            SS_among = NA_real_, SS_within = NA_real_, MS_among = NA_real_, MS_within = NA_real_,
            sigma2_among = NA_real_, sigma2_within = NA_real_ )
  N <- length( g )
  n <- as.numeric( table( g ) )
  K <- length( n )
  if( N < 2 || K < 2 || N <= K )
    return( out )
  SS_total <- sum( Dl ) / ( 2 * N )
  # Within-stratum sums of D in O(N^2), regardless of the number of strata.
  block <- rowsum( t( rowsum( Dl, g, reorder = TRUE ) ), g, reorder = TRUE )
  SS_within <- sum( diag( block ) / ( 2 * n ) )
  MS_among <- ( SS_total - SS_within ) / ( K - 1 )
  MS_within <- SS_within / ( N - K )
  n0 <- ( N - sum( n^2 ) / N ) / ( K - 1 )
  out[] <- c( N, K, K - 1, N - K, SS_total - SS_within, SS_within, MS_among, MS_within,
              ( MS_among - MS_within ) / n0, MS_within )
  out
}

# AMOVA quantities for every locus (plus Multilocus) using the rows 'rows' of
# the prepared data, with stratum labels 'labels' (one per prepared row).  The
# Multilocus row sums SS, MS and sigma-squared over the loci with an estimate;
# its N, K and df are kept only when they agree across those loci.
#' @keywords internal
#' @noRd
.amova_estimate <- function( prep, labels, rows = seq_along( labels ) ) {
  nl <- length( prep$loci )
  comp <- t( vapply( seq_len( nl ), function( i ) {
    k <- prep$keep[[i]]
    pos <- which( k %in% rows )
    .amova_components( prep$D[[i]][ pos, pos, drop = FALSE ], labels[ k[ pos ] ] )
  }, numeric( 10 ) ) )
  colnames( comp ) <- names( .amova_components( matrix( 0, 1, 1 ), "a" ) )
  if( nl > 1 ) {
    ok <- stats::complete.cases( comp )
    multi <- comp[ 1, ] * NA_real_
    if( any( ok ) ) {
      sub <- comp[ ok, , drop = FALSE ]
      multi <- colSums( sub )
      same <- function( v ) if( all( v == v[1] ) ) v[1] else NA_real_
      for( j in c( "N", "K", "df_among", "df_within" ) )
        multi[j] <- same( sub[ , j ] )
    }
    comp <- rbind( comp, multi )
  }
  phi <- comp[ , "sigma2_among" ] / ( comp[ , "sigma2_among" ] + comp[ , "sigma2_within" ] )
  phi[ !is.finite( phi ) ] <- NA_real_
  list( phi = unname( phi ), comp = comp )
}

# The per-locus results as an 'amova' object (see ?amova), with add-one
# permutation p-values when nperm > 0.  With nep, a Nep = 1/(2 Phi) column.
#' @keywords internal
#' @noRd
.amova_table <- function( prep, nperm = 0, name = "PhiST", stratum = "stratum", nep = FALSE ) {
  strata <- prep$strata
  obs <- .amova_estimate( prep, strata )
  loci <- prep$loci
  ret <- data.frame( Locus = if( length( loci ) > 1 ) c( loci, "Multilocus" ) else loci,
                     Phi = obs$phi,
                     sigma2_among = unname( obs$comp[ , "sigma2_among" ] ),
                     sigma2_within = unname( obs$comp[ , "sigma2_within" ] ),
                     stringsAsFactors = FALSE )
  names( ret )[2] <- name
  rownames( ret ) <- NULL
  if( nep )
    ret$Nep <- ifelse( !is.na( obs$phi ) & obs$phi > 0, 1 / ( 2 * obs$phi ), NA_real_ )

  ret$p.value <- NA_real_
  null_last <- NULL
  if( nperm > 0 ) {
    count <- numeric( nrow( ret ) )
    used  <- numeric( nrow( ret ) )
    null_last <- numeric( nperm )
    message("permuting ", appendLF = FALSE)
    for( rep in seq_len( nperm ) ) {
      if( rep %% 10 == 0 )
        message(".", appendLF = FALSE)
      null <- .amova_estimate( prep, sample( strata ) )$phi
      null_last[ rep ] <- null[ length( null ) ]
      ok <- !is.na( null ) & !is.na( obs$phi )
      count <- count + ( ok & null >= obs$phi )
      used  <- used + ok
    }
    message("")
    # Add-one permutation p-value (Phipson & Smyth 2010), as for the other modes.
    ret$p.value <- ifelse( is.na( obs$phi ), NA_real_, ( 1 + count ) / ( 1 + used ) )
  }

  # The analysis table for the last row: Multilocus, or the single locus.
  last <- obs$comp[ nrow( obs$comp ), ]
  s2 <- c( last[ "sigma2_among" ], last[ "sigma2_within" ] )
  tab <- data.frame( Df = c( last[ "df_among" ], last[ "df_within" ], last[ "N" ] - 1 ),
                     SS = c( last[ "SS_among" ], last[ "SS_within" ], last[ "SS_among" ] + last[ "SS_within" ] ),
                     MS = c( last[ "MS_among" ], last[ "MS_within" ], NA_real_ ),
                     Sigma2 = c( s2, sum( s2 ) ),
                     Percent = 100 * c( s2, sum( s2 ) ) / sum( s2 ),
                     row.names = c( "Among", "Within", "Total" ) )
  ns <- obs$comp[ seq_along( loci ), "N" ]
  structure( ret, class = c( "amova", "data.frame" ),
             amova = tab, statistic = name, stratum = stratum,
             n = range( ns, na.rm = TRUE ), strata = length( unique( strata ) ),
             loci = loci, nperm = nperm, permutations = null_last )
}

# Symmetric matrix of the last-row (Multilocus, or the single locus) Phi between
# every pair of strata, reusing the distances computed once for all rows.
#' @keywords internal
#' @noRd
.amova_pairwise <- function( prep ) {
  pops <- sort( unique( prep$strata ) )
  K <- length( pops )
  ret <- matrix( 0, nrow = K, ncol = K, dimnames = list( pops, pops ) )
  diag( ret ) <- NA
  for( i in seq_len( K - 1 ) ) {
    for( j in ( i + 1 ):K ) {
      rows <- which( prep$strata %in% pops[ c( i, j ) ] )
      phi <- .amova_estimate( prep, prep$strata, rows )$phi
      ret[i, j] <- ret[j, i] <- phi[ length( phi ) ]
    }
  }
  ret
}
