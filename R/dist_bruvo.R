#' Estimation of Bruvo's genetic distance
#'
#' Returns a pairwise distance matrix among individuals for microsatellite
#' data using the stepwise-mutation distance of Bruvo et al. (2004).  Alleles
#' are allele sizes, converted to repeat units by dividing by
#' \code{repeat_length}.  Two alleles \eqn{x} repeats apart are
#' \eqn{d_a = 1 - 2^{-x}} apart, so identical alleles are 0 apart, alleles one
#' repeat apart 0.5, and very different alleles approach 1.  At a locus with
#' ploidy \eqn{k} the distance between two individuals is the minimum, over all
#' pairings of their alleles, of \eqn{\frac{1}{k}\sum d_a}; the multilocus
#' distance is the mean over the loci typed in both individuals.  Distances
#' range from 0 to 1, and any ploidy is handled.
#'
#' A locus contributes nothing to a pair when either individual is missing
#' there, or when the two have different numbers of alleles (Bruvo's genome
#' addition and loss models are not implemented).  A pair with no such locus
#' in common gets \code{NA}.
#'
#' @param x A \code{locus} vector or a \code{data.frame} with \code{locus}
#'   columns whose alleles are numeric sizes (e.g. \code{"152:158"}).
#' @param repeat_length The repeat length (motif size) used to turn allele
#'   sizes into repeat counts: a single number for all loci, or a vector named
#'   by locus (default \code{1}, for alleles already coded as repeat counts).
#' @return A numeric distance matrix (N x N).
#' @references
#'   Bruvo R., Michiels N.K., D'Souza T.G. & Schulenburg H. (2004) A simple
#'   method for the calculation of microsatellite genotype distances
#'   irrespective of ploidy level. Molecular Ecology 13: 2101-2106.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @noRd
dist_bruvo <- function( x, repeat_length = 1 ) {

  if( is(x,"locus") )
    x <- data.frame( LOCUS=x )

  if( !is(x,"data.frame") )
    stop(paste("The function dist_bruvo() requires a locus vector or a data frame of locus vectors.  You passed a '",class(x), "' object.",sep=""))

  loci <- column_class(x, "locus")
  if( !length(loci) )
    stop("dist_bruvo() needs at least one locus column.")

  if( !is.numeric(repeat_length) || any(is.na(repeat_length)) || any(repeat_length <= 0) )
    stop("'repeat_length' must be positive numbers.")
  if( length(repeat_length) == 1 && is.null(names(repeat_length)) )
    repeat_length <- stats::setNames( rep(repeat_length, length(loci)), loci )
  else if( is.null(names(repeat_length)) || !all(loci %in% names(repeat_length)) )
    stop("'repeat_length' must be a single number or a vector named by locus, with an entry for every locus: ",
         paste(setdiff(loci, names(repeat_length)), collapse = ", "))

  N <- nrow(x)
  total <- matrix(0, N, N)
  used <- matrix(0, N, N)
  da <- function(a, b) 1 - 2^(-abs(outer(a, b, "-")))

  for( locus_name in loci ){
    A <- alleles( x[[locus_name]] )
    if( is.null(A) ) next
    A <- matrix(A, nrow = N)
    num <- suppressWarnings( as.numeric(A) )
    if( any( is.na(num) & !is.na(A) ) )
      stop(sprintf("Bruvo's distance needs numeric allele sizes; locus '%s' has non-numeric alleles.", locus_name))
    A <- matrix(num, nrow = N) / repeat_length[[locus_name]]
    k <- rowSums( !is.na(A) )

    # Pairs with the same ploidy: minimum mean allele distance over pairings.
    for( p in setdiff(unique(k), 0) ){
      idx <- which(k == p)
      Ap <- A[idx, seq_len(p), drop = FALSE]
      best <- matrix(Inf, length(idx), length(idx))
      for( perm in .permutations(p) ){
        d <- 0
        for( j in seq_len(p) )
          d <- d + da( Ap[, j], Ap[, perm[j]] )
        best <- pmin(best, d / p)
      }
      total[idx, idx] <- total[idx, idx] + best
      used[idx, idx] <- used[idx, idx] + 1
    }
  }

  ret <- total / used
  ret[used == 0] <- NA
  diag(ret) <- 0
  return( ret )
}

# All orderings of 1:n, as a list of integer vectors.
#' @keywords internal
#' @noRd
.permutations <- function( n ) {
  if( n <= 1 ) return( list(seq_len(n)) )
  out <- list()
  for( i in seq_len(n) )
    for( rest in .permutations(n - 1) )
      out[[length(out) + 1]] <- c(i, setdiff(seq_len(n), i)[rest])
  out
}
