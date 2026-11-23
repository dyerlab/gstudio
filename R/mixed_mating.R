#' Produce the next generation but allow for some level of selfing.
#'
#' Each adult is the mother of one mating; a fraction \code{s} of the
#'  matings are selfed and the rest are with a random father.  Every
#'  mating produces \code{N} offspring, each receiving one random allele
#'  from each parent at every locus (independent segregation).  The result
#'  is the same as calling \code{mate()} on the mating pairs, but all loci
#'  and offspring are drawn at once, so it is fast enough for
#'  forward-time simulation with \code{simulate_pop()}.
#' @param data A \code{data.frame} containing individuals with objects of
#'  type \code{locus}
#' @param N The number of offspring produced by each mating (default 1).
#' @param s The fraction of matings that are selfed.  The default, 0,
#'  pairs each adult as a mother with a father drawn by a random
#'  permutation of the adults, as \code{mate()} does without a father.
#'  With \code{s > 0}, mothers and fathers are drawn with replacement.
#' @return A \code{data.frame} of offspring.  Non-locus columns are copied
#'  from each offspring's mother; \code{ID}, if present, is renumbered
#'  \code{1:n}, a \code{Sex} column is drawn at random from its values, and
#'  an \code{OffID} column numbers the offspring within each mating.  An
#'  offspring is missing at a locus when either parent is.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#' data(arapat)
#' next_gen <- mixed_mating(arapat[1:10, ], N = 1, s = 0.2)
#' nrow(next_gen)
mixed_mating <- function( data, N = 1, s = 0 ) {
  if( !is.numeric( s ) || length( s ) != 1 || is.na( s ) || s < 0 || s > 1 )
    stop( "s must be bound on [0,1] at this time." )
  if( !is( data, "data.frame" ) )
    stop( "You must pass a data.frame object to use this function." )
  locus_names <- column_class( data, "locus" )
  if( !length( locus_names ) )
    stop( "Please pass at least one locus object to this function..." )
  if( !is.numeric( N ) || length( N ) != 1 || is.na( N ) || N < 1 )
    stop( "N must be a single number of offspring >= 1." )

  K <- nrow( data )
  N <- as.integer( N )

  # mating pairs (mothers and fathers as row indices)
  if( s == 0 ) {
    moms <- seq_len( K )
    dads <- sample.int( K )
  } else {
    n_self <- round( s * K )
    n_out  <- K - n_self
    selfed <- sample.int( K, size = n_self, replace = TRUE )
    moms   <- c( selfed, sample.int( K, size = n_out, replace = TRUE ) )
    dads   <- c( selfed, sample.int( K, size = n_out, replace = TRUE ) )
  }

  # one row per offspring
  moms  <- rep( moms, each = N )
  dads  <- rep( dads, each = N )
  n_off <- length( moms )

  ret <- data[moms, setdiff( names( data ), locus_names ), drop = FALSE]
  rownames( ret ) <- NULL
  if( "ID" %in% names( ret ) )
    ret$ID <- seq_len( n_off )
  if( "Sex" %in% names( ret ) )
    ret$Sex <- sample( unique( ret$Sex ), size = n_off, replace = TRUE )
  if( "OffID" %in% names( ret ) )
    ret$OffID <- rep( seq_len( N ), times = n_off / N )

  for( loc in locus_names ) {
    geno <- as.character( unclass( data[[loc]] ) )
    geno[is.na( geno )] <- ""
    ret[[loc]] <- .offspring_genotypes( geno[moms], geno[dads] )
  }

  ret[, names( data ), drop = FALSE]
}


# Internal: one offspring genotype per element from maternal and paternal
# genotype strings ("A:B", haploid "A", or "" for missing).  Each parent
# transmits one of its two alleles at random; alleles are sorted as
# locus() sorts them.
.offspring_genotypes <- function( mom, dad ) {
  n       <- length( mom )
  gamete  <- function( g ) {
    first  <- sub( ":.*$", "", g )
    second <- sub( "^[^:]*:", "", g )
    ifelse( stats::runif( n ) < 0.5, first, second )
  }
  a <- gamete( mom )
  b <- gamete( dad )
  swap <- !is.na( a ) & !is.na( b ) & a > b
  lo   <- ifelse( swap, b, a )
  hi   <- ifelse( swap, a, b )
  off  <- paste( lo, hi, sep = ":" )
  off[mom == "" | dad == ""] <- ""
  class( off ) <- "locus"
  off
}
