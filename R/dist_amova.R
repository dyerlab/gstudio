#' Estimation of AMOVA distance
#'
#' Returns a pairwise genetic distance matrix using the squared-distance model
#' from Smouse & Peakall (1999) for diploid genotypic data.  In this model
#' alleles are placed on unit-spaced vertices: AA and BB are 2 units apart,
#' AA and AB are 1 unit apart, and distances are squared before summing
#' (Sums of Squared Distances, SSxD; see Dyer et al. 2004).  The factor of 2
#' applied to the inner product rescales the allele-proportion vectors returned
#' by \code{to_mv} (which range 0–1) back to allele-substitution units before
#' squaring.
#'
#' For binary presence/absence markers (e.g. AFLPs) the Excoffier (1992)
#' formulation is appropriate; pass \code{binary = TRUE} to omit the factor
#' of 2 in that case.
#'
#' @param x A \code{locus} vector or a \code{data.frame} with \code{locus}
#'   columns.
#' @param binary Logical (default \code{FALSE}).  When \code{TRUE} the
#'   Excoffier (1992) formula for binary markers is used (no factor-of-2
#'   rescaling).
#' @return A numeric distance matrix (N × N).
#' @references
#'   Smouse P.E. & Peakall R. (1999) Spatial autocorrelation analysis of
#'   individual multiallele and multilocus genetic structure. Heredity 82:
#'   561–573.
#'
#'   Dyer R.J., Nason J.D. & Garrick R.C. (2004) Landscape modelling of gene
#'   flow: improved power using conditional genetic distance derived from the
#'   topology of population networks. Molecular Ecology 19: 3746–3759.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#' AA <- locus( c("A","A") )
#' AB <- locus( c("A","B") )
#' BB <- locus( c("B","B") )
#' AC <- locus( c("A","C") )
#' AD <- locus( c("A","D") )
#' BC <- locus( c("B","C") )
#' BD <- locus( c("B","D") )
#' CC <- locus( c("C","C") )
#' CD <- locus( c("C","D") )
#' DD <- locus( c("D","D") )
#' loci <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD)
#' D <- dist_amova( loci )
#' rownames(D) <- colnames(D) <- as.character(loci)
#' D
dist_amova <- function( x, binary = FALSE ) {

  if( is(x,"locus") )
    x <- data.frame( LOCUS=x )

  if( !is(x,"data.frame") )
    stop(paste("The function dist_amova() requires a locus vector or a data frame of locus vectors.  You passed a '",class(x), "' object.",sep=""))

  N <- dim(x)[1]
  ret <- matrix(0, ncol=N, nrow=N)

  # check for amova with mixed ploidy
  p <- ploidy(x)

  # make data vectors for 2Gener amova
  if( any( p$Ploidy != round(p$Ploidy))){
    stop("PLOIDY WARNING: Assuming 2Gener Approximation.")
    data <- FALSE

    if( any( p$Ploidy > 2))
      stop("As currently implemented, the 2gener amova distance is limited to diploid individuals.")

    loci <- p$Locus

    for( locus_name in loci){
      locus <- x[[locus_name]]
      freqs <- frequencies(locus)
      y <- to_mv( locus )
      p <- ploidy(locus)

      for( i in seq(1:N)[p==2]){
        f <- freqs$Frequency * y[i,]
        y[i,] <- f/sum(f)/2
      }

      if( is(data,"logical") )
        data <- y
      else
        data <- cbind( data, y)
    }

  }

  # make data vectors as adult AMOVA (Smouse & Peakall 1999 diploid model)
  else
    data <- to_mv( x, drop.allele=FALSE, leave.as.na = TRUE )

  # scale: diploid genotypes represented as allele proportions (0, 0.5, 1).
  # Multiplying by 2 restores the unit-allele-substitution scale so that
  # AA<->AB = 1 and AA<->BB = 4 in squared distance (SSxD model).
  # For binary markers (binary=TRUE) no rescaling is needed.
  scale <- if (binary) 1 else 2

  for( i in 1:N) {
    xi <- data[i,]
    for( j in 1:i) {
      if( i != j ) {
        yj <- data[j,]
        ret[i,j] <- ret[j,i] <- scale * sum( t(xi-yj) %*% (xi-yj) )
      }
    }
  }

  return( ret )
}
