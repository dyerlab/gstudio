#' AMOVA results from genetic_structure()
#'
#' \code{genetic_structure(mode = "PhiST")} and \code{genetic_structure(mode =
#'  "PhiFT")} return an object of class \code{c("amova", "data.frame")}.  It is
#'  the per-locus table described in \code{\link{genetic_structure}} (one row per
#'  locus and, with several loci, a \code{Multilocus} row), so it can be used as
#'  any \code{data.frame}.  It also carries the analysis of molecular variance
#'  table for the whole analysis: the \code{Multilocus} row when there are several
#'  loci, or the single locus.  To see the table for one locus, run the analysis on
#'  that locus (the \code{locus} argument of \code{genetic_structure()}).
#'
#' Printing shows that table, laid out as for \code{\link[stats]{aov}}: degrees of
#'  freedom, sums of squares and mean squares among and within strata, with the
#'  variance components (Sigma2) and their percentage of the total.  The
#'  multilocus sums of squares, mean squares and variance components are sums
#'  over loci.  When loci were genotyped in different numbers of individuals
#'  their degrees of freedom differ, and the multilocus Df are left blank.
#'  Significance comes from permutation (\code{nperm}), not an F distribution.
#' @param x,object An \code{amova} object.
#' @param digits Number of significant digits to print.
#' @param row.names,optional Ignored.
#' @param ... Ignored.
#' @return \code{anova()} returns the analysis table as an \code{"anova"}
#'  \code{data.frame}, with a \code{Phi} column holding the estimate (and its
#'  p-value) on the Among row.  \code{as.data.frame()} returns the per-locus table
#'  as a plain \code{data.frame}.
#' @name amova
#' @examples
#'  data(arapat)
#'  fit <- genetic_structure( arapat, stratum = "Species", mode = "PhiST", nperm = 99 )
#'  fit
#'  anova( fit )
#'  as.data.frame( fit )
#'  plot( fit )
#'  genetic_structure( arapat, stratum = "Species", mode = "PhiST", locus = "LTRS" )
NULL

.amova_labels <- function( x ) {
  if( identical( attr( x, "statistic" ), "PhiFT" ) ) c( "Among families", "Within families", "Total" )
  else c( "Among strata", "Within strata", "Total" )
}

#' @rdname amova
#' @export
print.amova <- function( x, digits = max( 3L, getOption( "digits" ) - 3L ), ... ) {
  tab <- attr( x, "amova" )
  if( is.null( tab ) ) {
    print( as.data.frame( x ), digits = digits )
    return( invisible( x ) )
  }
  stat <- attr( x, "statistic" )
  loci <- attr( x, "loci" )
  n <- attr( x, "n" )
  title <- if( stat == "PhiFT" ) "TwoGener Analysis of Molecular Variance (PhiFT)"
           else "Analysis of Molecular Variance (PhiST)"
  cat( "\n", title, "\n\n", sep = "" )
  cat( "Stratum: ", attr( x, "stratum" ), ";  ",
       if( n[1] == n[2] ) n[1] else paste0( n[1], "-", n[2] ), if( stat == "PhiFT" ) " gametes" else " individuals",
       " in ", attr( x, "strata" ), if( stat == "PhiFT" ) " families" else " strata", ";  ",
       if( length( loci ) == 1 ) paste( "locus", loci ) else paste( length( loci ), "loci" ), "\n\n", sep = "" )

  out <- tab
  rownames( out ) <- .amova_labels( x )
  f <- function( v, d ) { o <- rep( "", length( v ) ); ok <- !is.na( v ); o[ ok ] <- format( v[ ok ], digits = d ); o }
  shown <- data.frame( Df = ifelse( is.na( out$Df ), "", format( out$Df ) ),
                       SS = f( out$SS, digits ), MS = f( out$MS, digits ),
                       Sigma2 = f( out$Sigma2, digits ),
                       `% Var` = ifelse( is.na( out$Percent ), "", sprintf( "%.1f", out$Percent ) ),
                       row.names = rownames( out ), check.names = FALSE )
  print( shown, right = TRUE )

  last <- nrow( x )
  phi <- x[[ stat ]][ last ]
  p <- x$p.value[ last ]
  cat( "\n", stat, " = ", formatC( phi, digits = digits, format = "fg" ), sep = "" )
  if( stat == "PhiFT" && !is.null( x$Nep ) )
    cat( ";  Nep = ", if( is.na( x$Nep[ last ] ) ) "NA (PhiFT <= 0)" else formatC( x$Nep[ last ], digits = digits, format = "fg" ), sep = "" )
  nperm <- attr( x, "nperm" )
  if( !is.null( nperm ) && nperm > 0 )
    cat( ";  p = ", format.pval( p, digits = digits ), " (", nperm, " permutations)\n", sep = "" )
  else
    cat( "\n(no permutation test; set nperm to test ", stat, " = 0)\n", sep = "" )
  if( is.na( tab$Df[1] ) )
    cat( "Loci were genotyped in different numbers of individuals; Df are per locus, so not shown.\n" )
  if( any( tab$Sigma2[1:2] < 0, na.rm = TRUE ) )
    cat( "A negative variance component is an estimate of zero structure.\n" )
  cat( "\n" )
  invisible( x )
}

#' @rdname amova
#' @importFrom stats anova
#' @export
anova.amova <- function( object, ... ) {
  tab <- attr( object, "amova" )
  if( is.null( tab ) )
    stop( "This object no longer carries its AMOVA table; rerun genetic_structure()." )
  stat <- attr( object, "statistic" )
  last <- nrow( object )
  rownames( tab ) <- .amova_labels( object )
  names( tab )[ names( tab ) == "Percent" ] <- "% Var"
  tab$Phi <- c( object[[ stat ]][ last ], NA, NA )
  tab$`Pr(>=Phi)` <- c( object$p.value[ last ], NA, NA )
  names( tab )[ names( tab ) == "Phi" ] <- stat
  structure( tab, heading = paste0( "Analysis of Molecular Variance (", stat, ")\n" ),
             class = c( "anova", "data.frame" ) )
}

#' @rdname amova
#' @export
as.data.frame.amova <- function( x, row.names = NULL, optional = FALSE, ... ) {
  data.frame( lapply( x, identity ), stringsAsFactors = FALSE, check.names = FALSE )
}

#' @rdname amova
#' @param y Ignored.
#' @param bins Number of histogram bins (default 30).
#' @details \code{plot()} draws the permutation null distribution of the
#'  multilocus (or single-locus) Phi as a histogram, with the observed value as a
#'  vertical line, and returns the \code{ggplot}.  It needs \code{nperm > 0}.
#' @export
plot.amova <- function( x, y, ..., bins = 30 ) {
  null <- attr( x, "permutations" )
  if( is.null( null ) || !length( null ) )
    stop( "There is no permutation distribution to plot; rerun genetic_structure() with nperm > 0." )
  stat <- attr( x, "statistic" )
  last <- nrow( x )
  obs <- x[[ stat ]][ last ]
  p <- x$p.value[ last ]
  loci <- attr( x, "loci" )
  what <- if( length( loci ) == 1 ) paste( "locus", loci ) else paste( "multilocus,", length( loci ), "loci" )
  d <- data.frame( Phi = null[ !is.na( null ) ] )
  # Bin on the null values only; otherwise a distant observed value stretches
  # the bins across the whole axis.
  bw <- diff( range( d$Phi ) ) / bins
  if( !is.finite( bw ) || bw <= 0 ) bw <- max( abs( d$Phi ), 1e-3 ) / bins

  ggplot2::ggplot( d, ggplot2::aes( x = .data$Phi ) ) +
    ggplot2::geom_histogram( binwidth = bw, boundary = min( d$Phi ), fill = "grey55", colour = NA ) +
    ggplot2::geom_vline( xintercept = obs, colour = "#b2182b", linewidth = 0.9 ) +
    ggplot2::annotate( "label", x = obs, y = Inf, vjust = 1.3, hjust = if( obs > stats::median( d$Phi ) ) 1.05 else -0.05,
                       label = sprintf( "observed %s = %.3f, p = %s", stat, obs, format.pval( p, digits = 2 ) ),
                       size = 3.2, fill = "white" ) +
    ggplot2::labs( x = paste( stat, "under permutation" ), y = "Count",
                   title = paste0( "Permutation test of ", stat, " = 0" ),
                   subtitle = sprintf( "%d permutations of %s (%s)", length( null ), attr( x, "stratum" ), what ) ) +
    ggplot2::theme_minimal( base_size = 11 )
}
