#' Rarefaction of genetic diversity among strata
#'
#' Puts strata with unequal sample sizes on a common footing.  Each locus is
#'  rarefied to the smallest number of genotyped individuals found in any
#'  stratum at that locus: every larger stratum is resampled \code{nperm} times
#'  \strong{without replacement} at that size and the diversity statistic is
#'  estimated on each resample, giving its expected value and a percentile
#'  interval at the common size.  The smallest stratum is used as observed.
#' @param x A \code{data.frame} with \code{\link{locus}} columns, or a single
#'  \code{locus} vector (which needs \code{size}).
#' @param mode One or more per-locus diversity statistics, as in
#'  \code{\link{genetic_diversity}}: \code{"A"}, \code{"Ae"} (default),
#'  \code{"A95"}, \code{"He"}, \code{"Ho"}, \code{"Fis"}, \code{"Pe"},
#'  \code{"Sh"} or \code{"Pex"}.
#' @param stratum The column holding strata (default \code{"Population"}), or
#'  taken from \code{dplyr::group_by()} when \code{x} is grouped.  \code{NULL}
#'  treats all individuals as one sample, which then needs \code{size}.
#' @param loci The loci to use (default \code{NULL} uses all).
#' @param size Optional common sample size (individuals) to rarefy every locus
#'  to, instead of the smallest stratum.  Strata with fewer genotypes than
#'  \code{size} at a locus cannot be rarefied and are \code{NA}, with a warning.
#' @param nperm The number of resamples (default 999).
#' @param conf.level Coverage of the percentile interval (default 0.95).
#' @param na.rm How the \code{Multilocus} row treats loci that are \code{NA}
#'  in a stratum (no genotypes, or fewer than \code{size}).  \code{FALSE}
#'  (default) makes it \code{NA}, so strata are only compared over the same
#'  loci; \code{TRUE} combines the loci that are available.
#' @return A \code{data.frame} of class \code{"rarefaction_test"}, one row per
#'  stratum, locus and mode, with columns \code{Stratum} (if stratified),
#'  \code{Locus}, \code{Mode}, \code{N} (genotyped individuals), \code{size}
#'  (the rarefied size), \code{observed} (the statistic in the full stratum),
#'  \code{estimate} (the mean across resamples), and \code{conf.low} and
#'  \code{conf.high}, and the list-column \code{values} holding the
#'  \code{nperm} resampled values (hidden when printed).  With several loci a
#'  \code{Multilocus} row per stratum summarizes each resample across loci: the
#'  mean, except \eqn{1 - \sum H_o / \sum H_e} for \code{Fis} and
#'  \eqn{1 - \prod (1 - P)} for \code{Pex}; it is \code{NA} if any locus is,
#'  unless \code{na.rm = TRUE}.  It prints with a test-style
#'  header.
#' @seealso \code{\link{genetic_diversity}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#' data(arapat)
#' set.seed(1)
#' r <- rarefaction.test(arapat, mode = c("A", "Ae"), stratum = "Species",
#'                       loci = c("LTRS", "WNT"), nperm = 99)
#' r
#' # resampled values for the first row
#' hist(r$values[[1]])
#'
#' # one sample rarefied to a chosen size
#' rarefaction.test(arapat$LTRS, size = 20, nperm = 99)
rarefaction.test <- function( x, mode = "Ae", stratum = "Population", loci = NULL,
                              size = NULL, nperm = 999, conf.level = 0.95, na.rm = FALSE ) {
  dname <- deparse1(substitute(x))
  allowed <- c("A", "Ae", "A95", "He", "Ho", "Fis", "Pe", "Sh", "Pex")
  if( length(mode) < 1 || !all(tolower(mode) %in% tolower(allowed)) )
    stop(paste("rarefaction.test() takes the per-locus modes", paste(allowed, collapse = ", ")))
  mode <- allowed[ match(unique(tolower(mode)), tolower(allowed)) ]
  nperm <- as.integer(nperm)
  if( nperm < 1 )
    stop("'nperm' must be at least 1.")
  if( !is.null(size) && (length(size) != 1 || !is.numeric(size) || size < 1) )
    stop("'size' must be a single positive number.")

  if( is(x, "locus") ) {
    x <- data.frame( Locus = x )
    names(x) <- dname
    stratum <- NULL
  } else {
    if( !is(x, "data.frame") )
      stop("rarefaction.test() takes a data.frame with locus columns or a locus vector.")
    stratum <- .detect_stratum(x, stratum, default = "Population", explicit = !missing(stratum))
    x <- .plain_df(x)
  }

  all_loci <- column_class(x, "locus")
  if( length(all_loci) < 1 )
    stop("rarefaction.test() needs at least one locus.")
  if( is.null(loci) )
    loci <- all_loci
  if( !all(loci %in% all_loci) )
    stop("You are asking for loci not present in the data passed to rarefaction.test().")

  if( is.null(stratum) ) {
    if( is.null(size) )
      stop("Without strata there is nothing to rarefy to: give 'size'.")
    groups <- factor(rep("All", nrow(x)))
  } else {
    if( !(stratum %in% names(x)) )
      stop("Requested stratum is NOT in the data you passed to rarefaction.test().")
    groups <- factor(as.character(x[[stratum]]))
  }
  strata <- levels(groups)

  alpha <- (1 - conf.level) / 2
  stats_all <- c("A", "Ae", "A95", "He", "Ho", "Fis", "Pe", "Sh", "Pex")
  obs <- vals <- list()          # per locus: per stratum
  N <- sizes <- matrix( NA_real_, length(strata), length(loci), dimnames = list(strata, loci) )
  short <- character(0)

  for( locus in loci ) {
    cm <- .allele_counts_matrix( x[[locus]] )
    typed <- !is.na( x[[locus]] )
    n_s <- vapply( strata, function(s) sum(typed & groups == s), numeric(1) )
    sz <- if( !is.null(size) ) size else if( any(n_s > 0) ) min(n_s[n_s > 0]) else 0
    obs[[locus]] <- vals[[locus]] <- list()

    for( s in strata ) {
      idx <- which( typed & groups == s )
      n <- length(idx)
      o <- .rarefaction_stats( cm, matrix(1, 1, n), idx )
      if( n == 0 ) o[] <- NA_real_
      if( n < sz || sz < 1 ) {
        v <- matrix( NA_real_, nperm, length(stats_all), dimnames = list(NULL, stats_all) )
        short <- c(short, paste(s, locus))
      } else if( n == sz ) {
        v <- o[ rep(1, nperm), , drop = FALSE ]
      } else {
        pick <- as.vector( replicate( nperm, sample.int(n, sz) ) )
        S <- matrix( 0, nperm, n )
        S[ cbind( rep(seq_len(nperm), each = sz), pick ) ] <- 1
        v <- .rarefaction_stats( cm, S, idx )
      }
      obs[[locus]][[s]] <- o
      vals[[locus]][[s]] <- v
      N[s, locus] <- n
      sizes[s, locus] <- sz
    }
  }

  # Assemble one row (and one column of resampled values) per stratum x locus x mode
  rows <- list(); cols <- list()
  add <- function( s, locus, m, n, sz, o, v ) {
    rows[[length(rows) + 1]] <<- data.frame( Stratum = s, Locus = locus, Mode = m, N = n, size = sz,
                                             observed = o, stringsAsFactors = FALSE )
    cols[[length(cols) + 1]] <<- v
  }
  for( s in strata ) {
    for( locus in loci )
      for( m in mode )
        add( s, locus, m, N[s, locus], sizes[s, locus], obs[[locus]][[s]][1, m], vals[[locus]][[s]][, m] )
    if( length(loci) > 1 ) {
      for( m in mode ) {
        O <- sapply( loci, function(l) obs[[l]][[s]][1, c(m, "Ho", "He")] )
        V <- lapply( c(m, "Ho", "He"), function(k) sapply( loci, function(l) vals[[l]][[s]][, k] ) )
        V <- lapply( V, function(z) matrix(z, nrow = nperm) )
        if( na.rm ) {
          # drop loci with no value for this stratum and mode
          keep <- !is.na(O[1, ]) & colSums(is.na(V[[1]])) == 0
          O <- O[, keep, drop = FALSE]
          V <- lapply( V, function(z) z[, keep, drop = FALSE] )
        }
        if( ncol(O) == 0 ) {
          o <- NA_real_
          v <- rep(NA_real_, nperm)
        } else if( m == "Fis" ) {
          o <- 1 - sum(O["Ho", ]) / sum(O["He", ])
          v <- 1 - rowSums(V[[2]]) / rowSums(V[[3]])
        } else if( m == "Pex" ) {
          o <- 1 - prod(1 - O[1, ])
          v <- 1 - apply(1 - V[[1]], 1, prod)
        } else {
          o <- mean(O[1, ])
          v <- rowMeans(V[[1]])
        }
        add( s, "Multilocus", m, NA_real_, NA_real_, o, v )
      }
    }
  }

  ret <- do.call( rbind, rows )
  vals <- do.call( cbind, cols )
  ret$estimate  <- colMeans( vals )
  q <- function(p) apply( vals, 2, function(z) if( anyNA(z) ) NA_real_ else stats::quantile(z, p, names = FALSE) )
  ret$conf.low  <- q(alpha)
  ret$conf.high <- q(1 - alpha)
  if( is.null(stratum) )
    ret$Stratum <- NULL
  rownames(ret) <- NULL
  ret$values <- lapply( seq_len(ncol(vals)), function(j) unname(vals[, j]) )

  if( length(short) )
    warning(paste("Too few genotypes to rarefy, so NA:", paste(short, collapse = ", ")))

  attr(ret, "method") <- sprintf("Rarefaction without replacement (%d resamples)", nperm)
  attr(ret, "data.name") <- if( is.null(stratum) ) dname else paste(dname, "by", stratum)
  attr(ret, "size.rule") <- if( is.null(size) ) "smallest stratum at each locus" else paste(size, "individuals")
  attr(ret, "conf.level") <- conf.level
  class(ret) <- c("rarefaction_test", "data.frame")
  return( ret )
}


#' @rdname rarefaction.test
#' @param digits Significant digits to print.
#' @param ... Passed to \code{print.data.frame()}.
#' @export
print.rarefaction_test <- function( x, digits = getOption("digits") - 3, ... ) {
  if( !is.null(attr(x, "method")) ) {
    cat("\n\t", attr(x, "method"), "\n\n", sep = "")
    cat("data:  ", attr(x, "data.name"), "\n", sep = "")
    cat("size:  ", attr(x, "size.rule"), "\n", sep = "")
    cat(format(100 * attr(x, "conf.level")), " percent percentile interval of the resampled values\n\n", sep = "")
  }
  df <- x
  class(df) <- "data.frame"
  df$values <- NULL
  print(df, digits = digits, ...)
  invisible(x)
}


# Individual x allele copy-number matrix for a locus (missing rows are zero)
.allele_counts_matrix <- function( x ) {
  g <- as.character(x)
  g[ is.na(x) ] <- ""
  al <- strsplit(g, ":", fixed = TRUE)
  levs <- sort(unique(unlist(al)))
  levs <- levs[ levs != "" ]
  m <- matrix( 0, length(g), length(levs), dimnames = list(NULL, levs) )
  for( i in seq_along(al) )
    if( length(al[[i]]) )
      for( a in al[[i]] ) if( a != "" ) m[i, a] <- m[i, a] + 1
  het <- vapply( al, function(a) length(unique(a)) > 1, logical(1) )
  dip <- vapply( al, function(a) length(a) > 1, logical(1) )
  list( counts = m, het = as.numeric(het), dip = as.numeric(dip) )
}


# All statistics for each row of a selection matrix S over individuals 'idx'
.rarefaction_stats <- function( cm, S, idx ) {
  counts <- S %*% cm$counts[idx, , drop = FALSE]
  f <- counts / rowSums(counts)
  S2 <- rowSums(f^2); S3 <- rowSums(f^3); S4 <- rowSums(f^4); S5 <- rowSums(f^5)
  he <- 1 - S2
  ho <- as.vector( (S %*% cm$het[idx]) / (S %*% cm$dip[idx]) )
  lf <- ifelse( f > 0, log(f), 0 )
  cbind(
    A   = rowSums(counts > 0),
    Ae  = 1 / S2,
    A95 = rowSums(f >= 0.05),
    He  = he,
    Ho  = ho,
    Fis = 1 - ho / he,
    Pe  = rowSums(f * (1 - f)),
    Sh  = -rowSums(f * lf),
    # sum_i p(1-p)^2 - 1/2 sum_{i!=j} p_i^2 p_j^2 (4 - 3p_i - 3p_j), via power sums
    Pex = rowSums(f * (1 - f)^2) - 0.5 * ( 4 * (S2^2 - S4) - 6 * (S2 * S3 - S5) ) )
}
