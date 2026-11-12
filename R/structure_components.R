# Internal machinery shared by Gst(), Gst_prime() and Dest().
#
# Each locus is converted to an individual x allele frequency matrix once
# (to_mv.locus() is the expensive step); Hs and Ht are then recomputed from
# that matrix for any assignment of individuals to strata, which keeps the
# permutation tests cheap.

# Hs (mean within-stratum 1 - sum(p^2)) and Ht (pooled 1 - sum(p^2)) for one
# locus.  `inds` is the to_mv.locus() matrix (missing genotypes are rows of 0)
# and `strata` a factor of the same length.  Strata with no genotyped
# individuals at this locus are skipped.
.structure_hs_ht <- function( inds, strata ) {
  tot <- colSums( inds )
  n_tot <- sum( tot )
  ht <- if( n_tot > 0 ) 1 - sum( (tot / n_tot)^2 ) else NA_real_

  s <- rowsum( inds, as.integer( strata ), reorder = FALSE )
  n <- rowSums( s )
  keep <- n > 0
  hs <- if( any(keep) ) mean( 1 - rowSums( (s[keep, , drop = FALSE] / n[keep])^2 ) ) else NA_real_

  c( hs = hs, ht = ht )
}

# Turn raw Hs and Ht into an estimate.  k is the number of strata and
# n.harmonic the harmonic mean stratum size.  Returns the estimate together
# with the Hs and Ht it was built from (size-corrected when size.correct).
# A monomorphic locus (Ht = 0) has no estimate (NA) and reports Hs = Ht = 0.
.structure_estimate <- function( mode, hs, ht, k, n.harmonic, size.correct ) {

  if( is.na(ht) || is.na(hs) )
    return( c( est = NA_real_, hs = NA_real_, ht = NA_real_ ) )

  if( ht <= sqrt( .Machine$double.eps ) )
    return( c( est = NA_real_, hs = 0, ht = 0 ) )

  if( size.correct ) {
    hs <- (2 * n.harmonic) / (2 * n.harmonic - 1) * hs
    ht <- ht + hs / (2 * k * n.harmonic)
  }

  est <- switch( mode,
    gst       = 1 - hs / ht,
    # Hedrick (2005) G'ST
    gst_prime = (1 - hs / ht) * (k - 1 + hs) / ( (k - 1) * (1 - hs) ),
    # Jost (2008) D: [(Ht - Hs) / (1 - Hs)] * [k / (k - 1)]
    dest      = (ht - hs) / (1 - hs) * ( k / (k - 1) ),
    stop( "Unknown structure mode: ", mode )
  )

  c( est = est, hs = hs, ht = ht )
}

# Harmonic mean stratum size, from the stratum labels (all individuals).
.structure_n_harmonic <- function( strata ) {
  1 / mean( 1 / as.vector( table( strata ) ) )
}

# Multilocus estimate from per-locus results (vectors over loci).
#  - gst: Culley et al. (2001) ratio, 1 - mean(Hs) / mean(Ht)
#  - gst_prime, dest: harmonic mean of the per-locus estimates; loci with no
#    estimate (monomorphic) or a negative estimate are left out.
# Hs and Ht are reported as arithmetic means across loci.
.structure_multilocus <- function( mode, est, hs, ht ) {
  hs.tot <- mean( hs, na.rm = TRUE )
  ht.tot <- mean( ht, na.rm = TRUE )

  if( mode == "gst" ) {
    ml <- if( is.finite(ht.tot) && ht.tot > 0 ) 1 - hs.tot / ht.tot else NA_real_
  } else {
    v <- est[ is.finite(est) & est >= 0 ]
    ml <- if( length(v) ) 1 / mean( 1 / v ) else NA_real_
  }

  c( est = ml, hs = hs.tot, ht = ht.tot )
}

# Single-locus estimate plus (optionally) its permutation p-value.
.structure_single <- function( mode, x, strata, nperm, size.correct ) {
  strata <- factor( as.character( strata ) )
  k  <- nlevels( strata )
  nh <- .structure_n_harmonic( strata )
  inds <- to_mv.locus( x )

  hh  <- .structure_hs_ht( inds, strata )
  obs <- .structure_estimate( mode, hh[["hs"]], hh[["ht"]], k, nh, size.correct )

  P <- NA_real_
  if( nperm > 0 && !is.na( obs[["est"]] ) ) {
    null <- vapply( seq_len(nperm), function(b) {
      hp <- .structure_hs_ht( inds, sample( strata ) )
      .structure_estimate( mode, hp[["hs"]], hp[["ht"]], k, nh, size.correct )[["est"]]
    }, numeric(1) )
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
.structure_table <- function( mode, est_name, x, stratum, nperm, size.correct ) {

  locus_names <- column_class( x, "locus" )
  if( length(locus_names) == 0 )
    stop("You must pass some loci to this function")
  if( !(stratum %in% names(x)) )
    stop("You need to indicate a stratum variable column in the data.frame.")

  strata <- factor( as.character( x[[stratum]] ) )
  k  <- nlevels( strata )
  nh <- .structure_n_harmonic( strata )
  K  <- length( locus_names )
  mats <- lapply( locus_names, function(l) to_mv.locus( x[[l]] ) )

  per_locus <- function( s ) {
    vapply( mats, function(m) {
      hh <- .structure_hs_ht( m, s )
      .structure_estimate( mode, hh[["hs"]], hh[["ht"]], k, nh, size.correct )
    }, numeric(3) )
  }

  obs <- per_locus( strata )   # 3 x K: est, hs, ht
  ret <- data.frame( Locus = locus_names, Est = obs["est", ], Hs = obs["hs", ],
                     Ht = obs["ht", ], P = NA_real_, stringsAsFactors = FALSE )

  if( K > 1 ) {
    ml <- .structure_multilocus( mode, obs["est", ], obs["hs", ], obs["ht", ] )
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
        null[b, K + 1] <- .structure_multilocus( mode, p["est", ], p["hs", ], p["ht", ] )[["est"]]
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
