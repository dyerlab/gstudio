#' Estimation of PhiFT by TwoGener analysis of pollen pool structure
#'
#' TwoGener (Smouse et al. 2001) asks whether the pollen pools sampled by
#'  different mothers differ.  The maternal contribution is removed from every
#'  offspring with \code{minus_mom()}, leaving the paternal gamete, and the
#'  gametes are then partitioned among and within maternal families by an
#'  analysis of molecular variance exactly as for \code{PhiST}.
#'  PhiFT = sigma2_among / (sigma2_among + sigma2_within).
#'
#' When an offspring has the same heterozygous genotype as its mother, its
#'  paternal allele is ambiguous; it is kept and represented by the expected
#'  allele vector, weighted by the allele frequencies at that locus (the
#'  TwoGener approximation in \code{dist_amova()}).  A gamete that cannot be
#'  derived from its mother (a mismatch or mistyping) is set to missing at that
#'  locus, and offspring whose mother is not in the data are dropped.
#' @param x A \code{data.frame} of mothers and offspring with \code{locus}
#'  columns.  Mothers have \code{offspring == "0"}.
#' @param stratum The column identifying the maternal family (default "ID"),
#'  shared by a mother and her offspring.
#' @param offspring The column with offspring numbers, "0" for mothers
#'  (default "OffID").
#' @param nperm The number of permutations used to test PhiFT = 0 (default 0);
#'  gametes are permuted among maternal families.
#' @return A \code{data.frame} with Locus, PhiFT, sigma2_among and sigma2_within
#'  (and \code{P} when \code{nperm > 0}), laid out as for \code{PhiST}.
#' @references
#'   Smouse P.E., Dyer R.J., Westfall R.D. & Sork V.L. (2001) Two-generation
#'   analysis of pollen flow across a landscape. I. Male gamete heterogeneity
#'   among females. Evolution 55: 260-271.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @noRd
PhiFT <- function( x, stratum = "ID", offspring = "OffID", nperm = 0 ) {

  if( !is( x, "data.frame" ) )
    stop("This function requires you to pass it a data.frame of data...")
  if( !( stratum %in% names( x ) ) )
    stop("PhiFT needs a column identifying maternal families; '", stratum, "' is not in the data.")
  if( !( offspring %in% names( x ) ) )
    stop("PhiFT needs a column of offspring numbers (\"0\" for mothers); '", offspring, "' is not in the data.")

  loci <- column_class( x, "locus" )
  if( length( loci ) == 0 )
    stop("You need to pass this function a data.frame with some columns of type 'locus' to it...")

  is_mom <- as.character( x[[ offspring ]] ) == "0"
  moms <- x[ is_mom, , drop = FALSE ]
  if( anyDuplicated( as.character( moms[[ stratum ]] ) ) )
    stop("Each maternal family must have exactly one mother (offspring == \"0\").")

  has_mom <- as.character( x[[ stratum ]] ) %in% as.character( moms[[ stratum ]] )
  orphans <- sum( !is_mom & !has_mom )
  if( orphans > 0 )
    message( orphans, " offspring without a mother in the data were dropped." )
  x <- x[ is_mom | has_mom, , drop = FALSE ]

  # Paternal gametes: offspring minus the maternal contribution.
  off <- suppressMessages( minus_mom( x, MomCol = stratum, OffCol = offspring ) )
  if( nrow( off ) == 0 )
    stop("There are no offspring to analyze.")

  # A remaining diploid is usable only when it is the ambiguous case (the same
  # heterozygote as the mother); anything else is incompatible, so missing.
  mom_row <- match( as.character( off[[ stratum ]] ), as.character( moms[[ stratum ]] ) )
  bad <- 0
  for( l in loci ) {
    g <- as.character( off[[ l ]] )
    m <- as.character( moms[[ l ]] )[ mom_row ]
    diploid <- !is.na( off[[ l ]] ) & ploidy( off[[ l ]] ) > 1
    ambiguous <- diploid & is_heterozygote( off[[ l ]] ) & !is.na( m ) & g == m
    drop <- diploid & !ambiguous
    if( any( drop ) ) {
      bad <- bad + sum( drop )
      g[ drop ] <- ""
      off[[ l ]] <- locus( g, type = "separated" )
    }
  }
  if( bad > 0 )
    message( bad, " offspring genotypes incompatible with their mother were set to missing." )

  # The TwoGener approximation in dist_amova() warns about mixed ploidy; that is
  # expected here.
  ret <- withCallingHandlers(
    PhiST( off, stratum = stratum, nperm = nperm ),
    warning = function( w ) {
      if( grepl( "2Gener", conditionMessage( w ) ) )
        invokeRestart( "muffleWarning" )
    })
  names( ret )[ names( ret ) == "PhiST" ] <- "PhiFT"
  return( ret )
}
