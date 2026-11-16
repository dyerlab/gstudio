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
#'  allele vector, weighted by the gamete allele frequencies at that locus
#'  (\code{dist_amova(pollen_pool = TRUE)}).  A gamete that cannot be derived from
#'  its mother (a mismatch or mistyping) is set to missing at that locus, and
#'  offspring whose mother is not in the data are dropped.
#'
#' A mother not genotyped at a locus has her genotype reconstructed from her
#'  offspring (\code{.infer_maternal()}).  When her offspring do not identify it
#'  clearly, her offspring are set to missing at that locus.  Every reconstruction
#'  is reported in a message and in the \code{"maternal_inference"} attribute.
#' @param x A \code{data.frame} of mothers and offspring with \code{locus}
#'  columns.  Mothers have \code{offspring == "0"}.
#' @param stratum The column identifying the maternal family (default "ID"),
#'  shared by a mother and her offspring.
#' @param offspring The column with offspring numbers, "0" for mothers
#'  (default "OffID").
#' @param nperm The number of permutations used to test PhiFT = 0 (default 0);
#'  gametes are permuted among maternal families.
#' @param pairwise If \code{TRUE}, a matrix of PhiFT between every pair of
#'  maternal families, from the gametes and distances of the whole sample.
#' @return An \code{amova} object (see \code{?amova}) with Locus, PhiFT,
#'  sigma2_among, sigma2_within, Nep = 1 / (2 PhiFT), the effective number of
#'  pollen donors (NA when PhiFT <= 0), and p.value.
#' @references
#'   Smouse P.E., Dyer R.J., Westfall R.D. & Sork V.L. (2001) Two-generation
#'   analysis of pollen flow across a landscape. I. Male gamete heterogeneity
#'   among females. Evolution 55: 260-271.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @noRd
PhiFT <- function( x, stratum = "ID", offspring = "OffID", nperm = 0, pairwise = FALSE ) {

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

  # Fill in maternal genotypes missing at a locus from the offspring.
  inferred <- .infer_maternal( x, stratum = stratum, offspring = offspring, loci = loci )
  x <- inferred$x
  moms <- x[ as.character( x[[ offspring ]] ) == "0", , drop = FALSE ]

  # Paternal gametes: offspring minus the maternal contribution.
  off <- suppressMessages( minus_mom( x, MomCol = stratum, OffCol = offspring ) )
  if( nrow( off ) == 0 )
    stop("There are no offspring to analyze.")

  # A remaining diploid is usable only when it is the ambiguous case (the same
  # heterozygote as the mother).  With a mother still missing at the locus (already
  # reported) it is dropped; otherwise it is incompatible with her.
  mom_row <- match( as.character( off[[ stratum ]] ), as.character( moms[[ stratum ]] ) )
  bad <- 0
  for( l in loci ) {
    g <- as.character( off[[ l ]] )
    m <- as.character( moms[[ l ]] )[ mom_row ]
    mom_missing <- is.na( moms[[ l ]] )[ mom_row ]
    diploid <- !is.na( off[[ l ]] ) & ploidy( off[[ l ]] ) > 1
    ambiguous <- diploid & !mom_missing & is_heterozygote( off[[ l ]] ) & g == m
    drop <- ( diploid & !ambiguous ) | ( mom_missing & !is.na( off[[ l ]] ) )
    bad <- bad + sum( drop & !mom_missing )
    if( any( drop ) ) {
      g[ drop ] <- ""
      off[[ l ]] <- locus( g, type = "separated" )
    }
  }
  if( bad > 0 )
    message( bad, " offspring genotypes incompatible with their mother were set to missing." )

  prep <- .amova_prep( off, stratum, pollen_pool = TRUE )
  if( pairwise )
    return( .amova_pairwise( prep ) )

  ret <- .amova_table( prep, nperm = nperm, name = "PhiFT", stratum = stratum, nep = TRUE )
  if( !is.null( inferred$report ) )
    attr( ret, "maternal_inference" ) <- inferred$report
  ret
}


# Reconstruct maternal genotypes missing at a locus from her diploid offspring.
# For each candidate genotype a:b the likelihood of the offspring is
#   prod_o 1/2 [ P(o | a transmitted) + P(o | b transmitted) ],
# where the paternal allele is drawn from the allele frequencies at the locus
# (mothers and offspring pooled).  The best genotype is used when it is at least
# 'min_lr' times as likely as the next best; otherwise the mother stays missing.
# Returns the data with mothers filled in and a report (NULL if nothing missing).
#' @keywords internal
#' @noRd
.infer_maternal <- function( x, stratum, offspring, loci, min_lr = 20 ) {
  is_mom <- as.character( x[[ offspring ]] ) == "0"
  fam <- as.character( x[[ stratum ]] )
  report <- list()

  for( l in loci ) {
    missing_moms <- which( is_mom & is.na( x[[ l ]] ) )
    if( !length( missing_moms ) ) next
    fr <- frequencies( x[[ l ]] )
    p <- stats::setNames( fr$Frequency, fr$Allele )
    al <- names( p )
    cand <- if( length( al ) ) unique( t( apply( expand.grid( al, al, stringsAsFactors = FALSE ), 1, sort ) ) ) else matrix( character(), 0, 2 )
    new_g <- as.character( x[[ l ]] )

    for( r in missing_moms ) {
      # A mother without offspring contributes nothing to PhiFT.
      if( !any( !is_mom & fam == fam[r] ) ) next
      kids <- which( !is_mom & fam == fam[r] & !is.na( x[[ l ]] ) )
      kids <- kids[ ploidy( x[[ l ]][ kids ] ) == 2 ]
      og <- do.call( rbind, strsplit( as.character( x[[ l ]][ kids ] ), ":" ) )
      best <- NA_character_; second <- NA_character_; lr <- NA_real_
      if( length( kids ) && nrow( cand ) ) {
        trans <- function( m ) ( og[ , 1 ] == m ) * p[ og[ , 2 ] ] + ( og[ , 2 ] == m ) * p[ og[ , 1 ] ] -
          ( og[ , 1 ] == m & og[ , 2 ] == m ) * p[ og[ , 1 ] ]
        ll <- apply( cand, 1, function( g ) sum( log( 0.5 * ( trans( g[1] ) + trans( g[2] ) ) ) ) )
        o <- order( ll, decreasing = TRUE )
        best <- paste( cand[ o[1], ], collapse = ":" )
        if( length( o ) > 1 ) second <- paste( cand[ o[2], ], collapse = ":" )
        lr <- if( !is.finite( ll[ o[1] ] ) ) NA_real_ else if( length( o ) > 1 ) exp( ll[ o[1] ] - ll[ o[2] ] ) else Inf
      }
      used <- !is.na( lr ) && lr >= min_lr
      if( used ) new_g[r] <- best
      report[[ length( report ) + 1 ]] <- data.frame(
        Family = fam[r], Locus = l, Offspring = length( kids ),
        Genotype = if( is.na( lr ) ) NA_character_ else best,
        Next = second, LR = lr, Used = used, stringsAsFactors = FALSE )
    }
    x[[ l ]] <- locus( new_g, type = "separated" )
  }

  if( !length( report ) )
    return( list( x = x, report = NULL ) )
  report <- do.call( rbind, report )

  fmt <- function( v ) ifelse( is.infinite( v ), "Inf", formatC( v, format = "g", digits = 3 ) )
  support <- ifelse( is.infinite( report$LR ), "the only genotype compatible with them",
                     paste( fmt( report$LR ), "times as likely as", report$Next ) )
  lines <- ifelse( report$Used,
    sprintf( "  %s at %s: inferred %s from %d offspring (%s).",
             report$Family, report$Locus, report$Genotype, report$Offspring, support ),
    ifelse( report$Offspring == 0,
      sprintf( "  %s at %s: NOT inferred; none of her offspring are genotyped there.",
               report$Family, report$Locus ),
    ifelse( is.na( report$LR ),
      sprintf( "  %s at %s: NOT inferred; her %d offspring there are compatible with no single maternal genotype (check for mistyping).",
               report$Family, report$Locus, report$Offspring ),
      sprintf( "  %s at %s: NOT inferred; best %s from %d offspring is only %s times as likely as %s.",
               report$Family, report$Locus, report$Genotype, report$Offspring, fmt( report$LR ), report$Next ) ) ) )
  message( nrow( report ), " maternal genotypes are missing; reconstructing them from the offspring ",
           "(used when at least ", min_lr, " times as likely as the next best genotype):\n",
           paste( lines, collapse = "\n" ),
           if( any( !report$Used ) ) paste0( "\nOffspring of mothers not inferred are set to missing at that locus. ",
                                            "Genotype those mothers, or add offspring, to include them." ) else "",
           "\nSee attr(result, \"maternal_inference\")." )
  list( x = x, report = report )
}
