#' Make loci
#' 
#' This function makes a vector of locus objects given a frequency array and an 
#'  estimate of the inbreeding frequency. Given that it is not possible to 'perfectly' 
#'  make the right distribution of genotypes for all given N & F combinations, this will
#'  work to provide as close of an approximation as possible.
#' @param x A \code{data.frame} with Allele and Frequency columns.  The Locus column specifies
#'  the names of the alleles to be used and the Frequency one gives their occurrence.
#' @param F A numeric value for the inbeeding statistic (default=0)
#' @param N The number of loci to return (default=20)
#' @return A vector of \code{locus} objects.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#'  x <- data.frame(Allele=c("A","B"), Frequency=c(0.5,0.5))
#'  make_loci(x)
#'  make_loci(x,F=1)

make_loci <- function( x, N=20, F=0 ) {
  alleles <- x$Allele
  freqs <- x$Frequency
  if( sum(freqs) != 1){
    d <- (1.0 - sum(freqs))/length(alleles)
    warning( paste( "Your allele frequencies do not add to 1.0.  The difference of ",
                    d, " will be partioned across all noted alleles", sep=""))
    freqs <- freqs + d
  }
  K <- length(alleles)
  P <- rep(0,(K + K*(K-1)/2) )
  G <- rep("",length(P))
  ctr <- 1
  for( i in 1:K){
    for( j in i:K){
      G[ctr] <- paste(alleles[i],alleles[j],sep=":")
      if( i==j ) {
        P[ctr] <- (freqs[i]^2)*(1-F) + (F * freqs[i])
      }
      else
        P[ctr] <- (2*freqs[i] * freqs[j]) * (1-F) 
      ctr <- ctr+1
    }
  }
  
  P <- round(P*N)
  loci <- c()
  for( i in 1:length(P)){
    loci <- c( loci, rep(G[i],times=P[i]))
  }
  loci <- locus( loci, type="separated")
  if( length(loci) > N )
    loci <- loci[1:N] 
  else if(length(loci) < N ) {
    loci <- c( loci, sample(loci,size=N-length(loci),replace = TRUE))
  }
  return( loci )
}