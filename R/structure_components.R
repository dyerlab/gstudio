# Internal machinery shared by Gst(), Gst_prime() and Dest().
#
# Each locus is converted to an individual x allele frequency matrix once
# (to_mv.locus() is the expensive step); Hs and Ht are then recomputed from
# that matrix for any assignment of individuals to strata, which keeps the
# permutation tests cheap.  The estimators and the reasons for each choice are
# documented under "Gst, Gst_prime and Dest" in ?genetic_structure.

# Raw (uncorrected) Hs and Ht for one locus, plus the number of strata k and
# the harmonic mean stratum size n.harmonic that the sample-size correction and
# the k-dependent estimators need.  `inds` is the to_mv.locus() matrix (missing
# genotypes are rows of 0) and `strata` a factor of the same length.
#
# Everything is per locus: only individuals genotyped at this locus (and with a
# non-missing stratum) count, so k is the number of strata with at least one
# genotyped individual and n.harmonic is the harmonic mean of the genotyped
# counts in those strata.
#  - Hs: unweighted mean over strata of 1 - sum(p^2)
#  - Ht: 1 - sum(pbar^2), pbar the unweighted mean of the stratum frequencies
#    (Nei 1973, 1987), so every stratum counts equally whatever its sample size.
.structure_hs_ht <- function( inds, strata ) {
  typed <- rowSums( inds ) > 0 & !is.na( strata )
  if( !any( typed ) )
    return( c( hs = NA_real_, ht = NA_real_, k = 0, n.harmonic = NA_real_ ) )

  g <- as.integer( strata[typed] )
  s <- rowsum( inds[typed, , drop = FALSE], g, reorder = FALSE )
  n <- as.vector( rowsum( rep( 1, sum(typed) ), g, reorder = FALSE ) )

  p <- s / rowSums( s )
  hs <- mean( 1 - rowSums( p^2 ) )
  ht <- 1 - sum( colMeans( p )^2 )

  c( hs = hs, ht = ht, k = nrow( p ), n.harmonic = 1 / mean( 1 / n ) )
}

# Turn raw Hs and Ht into an estimate.  k is the number of strata and
# n.harmonic the harmonic mean stratum size (both for this locus).  Returns the
# estimate together with the Hs and Ht it was built from (size-corrected when
# size.correct).  A monomorphic locus (Ht = 0) has no estimate (NA) and reports
# Hs = Ht = 0; a locus genotyped in fewer than two strata has no estimate.
.structure_estimate <- function( mode, hs, ht, k, n.harmonic, size.correct ) {

  if( is.na(ht) || is.na(hs) )
    return( c( est = NA_real_, hs = NA_real_, ht = NA_real_ ) )

  if( ht <= sqrt( .Machine$double.eps ) )
    return( c( est = NA_real_, hs = 0, ht = 0 ) )

  # Jost (2008) eqs. 12-13, the Nei & Chesser (1983) correction without the
  # observed heterozygosity term (2N gene copies per stratum: diploid).
  if( size.correct ) {
    hs <- (2 * n.harmonic) / (2 * n.harmonic - 1) * hs
    ht <- ht + hs / (2 * k * n.harmonic)
  }

  if( k < 2 )
    return( c( est = NA_real_, hs = hs, ht = ht ) )

  c( est = .structure_formula( mode, hs, ht, k ), hs = hs, ht = ht )
}

# The estimators themselves, from (corrected) Hs, Ht and k.
.structure_formula <- function( mode, hs, ht, k ) {
  switch( mode,
    gst       = 1 - hs / ht,
    # Hedrick (2005) G'ST
    gst_prime = (1 - hs / ht) * (k - 1 + hs) / ( (k - 1) * (1 - hs) ),
    # Jost (2008) D: [(Ht - Hs) / (1 - Hs)] * [k / (k - 1)]
    dest      = (ht - hs) / (1 - hs) * ( k / (k - 1) ),
    stop( "Unknown structure mode: ", mode )
  )
}

# Multilocus estimate from per-locus results (vectors over loci).  k is the
# number of strata in the whole data set.
#  - gst: always the Culley et al. (2001) ratio of averages, 1 - mean(Hs)/mean(Ht)
#  - gst_prime, dest with multilocus = "harmonic": harmonic mean of the
#    per-locus estimates; loci with no estimate (monomorphic) or a negative
#    estimate are left out.
#  - gst_prime, dest with multilocus = "average": the estimator applied to the
#    across-locus means of Hs and Ht (mmod's global values, hierfstat's Dest).
# Hs and Ht are reported as arithmetic means across loci (monomorphic loci
# contribute Hs = Ht = 0, as in mmod and hierfstat).
.structure_multilocus <- function( mode, est, hs, ht, k, multilocus = "harmonic" ) {
  hs.tot <- mean( hs, na.rm = TRUE )
  ht.tot <- mean( ht, na.rm = TRUE )
  ok <- is.finite(ht.tot) && ht.tot > 0 && k >= 2

  if( mode == "gst" || multilocus == "average" ) {
    ml <- if( ok ) .structure_formula( mode, hs.tot, ht.tot, k ) else NA_real_
  } else {
    v <- est[ is.finite(est) & est >= 0 ]
    ml <- if( length(v) ) 1 / mean( 1 / v ) else NA_real_
  }

  c( est = ml, hs = hs.tot, ht = ht.tot )
}

.structure_multilocus_arg <- function( multilocus ) {
  match.arg( multilocus[1], c( "harmonic", "average" ) )
}

# Single-locus estimate plus (optionally) its permutation p-value.
.structure_single <- function( mode, x, strata, nperm, size.correct ) {
  strata <- factor( as.character( strata ) )
  inds <- to_mv.locus( x )

  est_of <- function( s ) {
    hh <- .structure_hs_ht( inds, s )
    .structure_estimate( mode, hh[["hs"]], hh[["ht"]], hh[["k"]], hh[["n.harmonic"]], size.correct )
  }
  obs <- est_of( strata )

  P <- NA_real_
  if( nperm > 0 && !is.na( obs[["est"]] ) ) {
    null <- vapply( seq_len(nperm), function(b) est_of( sample( strata ) )[["est"]], numeric(1) )
    null <- null[ !is.na(null) ]
    # Add-one permutation p-value: (1 + #{perm >= obs}) / (1 + B), strictly
    # positive (Phipson & Smyth 2010, Stat. Appl. Genet. Mol. Biol. 9:Article39).
    P <- ( 1 + sum( null >= obs[["est"]] ) ) / ( 1 + length(null) )
  }

  list( est = obs[["est"]], hs = obs[["hs"]], ht = obs[["ht"]], P = P )
}

# Data-frame front end used by Gst(), Gst_prime() and Dest().  Returns one row
# per locus and, with more than one locus, a Multilocus row.  `est_name` is
# the name of the estimate column.  The Multilocus p-value comes from
# permuting strata across individuals (the same permutation at every locus).
.structure_table <- function( mode, est_name, x, stratum, nperm, size.correct, multilocus = "harmonic" ) {

  multilocus <- .structure_multilocus_arg( multilocus )
  locus_names <- column_class( x, "locus" )
  if( length(locus_names) == 0 )
    stop("You must pass some loci to this function")
  if( !(stratum %in% names(x)) )
    stop("You need to indicate a stratum variable column in the data.frame.")

  strata <- factor( as.character( x[[stratum]] ) )
  k  <- nlevels( strata )
  K  <- length( locus_names )
  mats <- lapply( locus_names, function(l) to_mv.locus( x[[l]] ) )

  per_locus <- function( s ) {
    vapply( mats, function(m) {
      hh <- .structure_hs_ht( m, s )
      .structure_estimate( mode, hh[["hs"]], hh[["ht"]], hh[["k"]], hh[["n.harmonic"]], size.correct )
    }, numeric(3) )
  }

  obs <- per_locus( strata )   # 3 x K: est, hs, ht
  ret <- data.frame( Locus = locus_names, Est = obs["est", ], Hs = obs["hs", ],
                     Ht = obs["ht", ], P = NA_real_, stringsAsFactors = FALSE )

  if( K > 1 ) {
    ml <- .structure_multilocus( mode, obs["est", ], obs["hs", ], obs["ht", ], k, multilocus )
    ret <- rbind( ret, data.frame( Locus = "Multilocus", Est = ml[["est"]],
                                   Hs = ml[["hs"]], Ht = ml[["ht"]], P = NA_real_,
                                   stringsAsFactors = FALSE ) )
  }

  if( nperm > 0 ) {
    null <- matrix( NA_real_, nrow = nperm, ncol = nrow(ret) )
    for( b in seq_len(nperm) ) {
      p <- per_locus( sample( strata ) )
      null[b, 1:K] <- p["est", ]
      if( K > 1 )
        null[b, K + 1] <- .structure_multilocus( mode, p["est", ], p["hs", ], p["ht", ], k, multilocus )[["est"]]
    }
    for( j in seq_len( nrow(ret) ) ) {
      o <- ret$Est[j]
      v <- null[, j]
      v <- v[ !is.na(v) ]
      if( !is.na(o) && length(v) )
        ret$P[j] <- ( 1 + sum( v >= o ) ) / ( 1 + length(v) )
    }
  }

  names(ret)[2] <- est_name
  rownames(ret) <- NULL
  ret
}
