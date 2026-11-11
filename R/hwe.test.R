#' Test for Hardy-Weinberg Equilibrium
#'
#' Tests observed genotype counts against Hardy-Weinberg expectations with a
#'  chi-square goodness-of-fit test, in the style of \code{\link[stats]{chisq.test}}.
#'  When any expected genotype count is below five, a continuity correction of
#'  0.5 is applied and the method says so.
#' @param x A \code{locus} vector, or a \code{data.frame} with one or more
#'  \code{locus} columns.
#' @param stratum Optional name of a column of \code{x} holding strata; each
#'  locus is then tested within each stratum.  If not given, it is taken from
#'  \code{dplyr::group_by()} when \code{x} is grouped.
#' @param loci The loci to test (default \code{NULL} uses all).
#' @param suppress_warnings Silence the small-sample warnings (fewer than 50
#'  genotypes or fewer than 5 expected), e.g. when testing simulated data.
#' @return For a single test (one locus, no stratum), an object of class
#'  \code{"htest"} that prints like \code{chisq.test()} and works with
#'  \code{broom::tidy()}, with components \code{statistic} (\code{X-squared}),
#'  \code{parameter} (\code{df}), \code{p.value}, \code{method},
#'  \code{data.name}, and the genotype \code{observed} and \code{expected}
#'  counts.  For several tests, a \code{data.frame} with one row per test:
#'  \code{Stratum} (if given), \code{Locus}, \code{statistic}, \code{df},
#'  \code{p.value} and \code{method}.
#' @seealso \code{\link{genotype_frequencies}}
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#'  data(arapat)
#'  hwe.test( arapat$LTRS )
#'  sonora <- arapat[ arapat$Species=="Mainland",]
#'  hwe.test( sonora )
#'  hwe.test( arapat, stratum="Species", loci="LTRS", suppress_warnings=TRUE )
hwe.test <- function( x, stratum=NULL, loci=NULL, suppress_warnings=FALSE ){

  dname <- deparse1(substitute(x))

  if( is(x,"locus") ) {
    x <- data.frame( Locus=x )
    names(x) <- dname
  }
  if( !is(x,"data.frame") )
    stop("hwe.test() takes a locus vector or a data.frame with locus columns.")

  stratum <- .detect_stratum(x, stratum, default = NULL, explicit = !missing(stratum))
  x <- .plain_df(x)

  all_loci <- column_class( x, "locus" )
  if( length(all_loci) < 1 )
    stop("hwe.test() needs a data.frame with at least one locus column.")
  if( is.null(loci) )
    loci <- all_loci
  if( !all(loci %in% all_loci) )
    stop("You are asking for loci not present in the data passed to hwe.test().")

  # A single test: return the htest itself
  if( is.null(stratum) && length(loci) == 1 ) {
    res <- .hwe_chisq( x[[loci]], label=loci, suppress_warnings=suppress_warnings )
    res$data.name <- if( identical(loci, dname) ) dname else paste0(dname, "$", loci)
    return( res )
  }

  if( !is.null(stratum) ) {
    if( !(stratum %in% names(x)) )
      stop("Requested stratum is NOT in the data you passed to hwe.test().")
    pops <- partition( x, stratum=stratum )
  } else {
    pops <- list( x )
  }

  rows <- list()
  for( pop in seq_along(pops) ) {
    for( locus in loci ) {
      label <- if( is.null(stratum) ) locus else paste(locus, "in", names(pops)[pop])
      r <- .hwe_chisq( pops[[pop]][[locus]], label=label, suppress_warnings=suppress_warnings )
      row <- data.frame( Locus=locus, statistic=unname(r$statistic),
                         df=unname(r$parameter), p.value=r$p.value,
                         method=r$method, stringsAsFactors=FALSE )
      if( !is.null(stratum) )
        row <- cbind( Stratum=names(pops)[pop], row, stringsAsFactors=FALSE )
      rows[[length(rows) + 1]] <- row
    }
  }
  ret <- do.call( rbind, rows )
  rownames(ret) <- NULL
  return( ret )
}


# Chi-square HWE test on one locus vector, returned as an htest
.hwe_chisq <- function( x, label, suppress_warnings=FALSE ) {
  data <- genotype_frequencies( x, suppress_warnings=TRUE )
  alleles <- unique( as.character( alleles(x) ) )
  alleles <- alleles[ !is.na(alleles) ]
  ell <- length( alleles )

  if( sum(data$Observed) < 50 && !suppress_warnings )
    warning(paste("Under 50 samples for", label, "this may not be a good approximation."))

  correction <- 0
  if( any(data$Expected < 5) ) {
    if( !suppress_warnings )
      warning(paste("Fewer than 5 genotypes expected at", label, "consider collapsing alleles."))
    correction <- 0.5
  }

  chi <- sum( (abs(data$Observed - data$Expected) - correction)^2 / data$Expected )
  df <- ell * (ell - 1) / 2

  res <- list(
    statistic = c( "X-squared" = chi ),
    parameter = c( df = df ),
    p.value   = stats::pchisq( chi, df=df, lower.tail=FALSE ),
    method    = paste0( "Chi-square test for Hardy-Weinberg equilibrium",
                        if( correction > 0 ) " with continuity correction" else "" ),
    data.name = label,
    observed  = stats::setNames( data$Observed, data$Genotype ),
    expected  = stats::setNames( data$Expected, data$Genotype ) )
  class(res) <- "htest"
  return( res )
}
