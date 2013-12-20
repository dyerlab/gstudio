#' Estimates pair-wise relatedness
#' 
#' This function returns single relatedness estimates as a 
#'  pairwise matrix.
#' @param loci An object of type \code{locus} 
#' @param mode The kind of relatedness to be estimated.  Currently
#'  Ritland96 (the default) and LynchRitland are available.
#' @param freqs An optional \code{data.frame} (as returned by the
#'  function \code{frequencies()} with allele frequencies).  If this 
#'  is not provided, it will be estimated from the locus.  This allows
#'  you to estimate relatedness among subsets of individuals using 
#'  more global measures of relatedness.
#' @return A matrix of pairwise relatedness estimates
#' @note This only works on diploid data and will return NA for any 
#'  comparison of missing genotypes.
#' @export
#' @examples
#' loci <- c( locus(1:2), locus(c(2,2)), locus(1:2) )
#' genetic_relatedness( loci )
#' genetic_relatedness( loci, freqs = data.frame( Allele=c("1","2"), Frequency=c(0.5,0.5)))
genetic_relatedness <- function( loci, mode=c("Ritland96","LynchRitland")[1], freqs=NULL ) {
  if( missing(loci) )
    stop("Cannot use this function without actually passing it some loci.")
  if( !is(loci,"locus"))
    stop("You need to pass objects of type 'locus' to this function.")
  if( !(mode %in% c("Ritland96","LynchRitland")))
    stop("Unrecognized relatedness measure")
  if( is.null(freqs) )
    freqs <- frequencies( loci )
  n <- length( freqs$Allele )
  if( n < 6 & mode=="LynchRitland")
    warning( "You should probably not use Lynch & Ritland estimator for loci with fewer than 6 alleles...")
  
  
  
  # define the individual functions
  .rit96 <- function(dij,dik,dil,djk,djl,pi,pj,n){
    top <- (dik+dil)/pi + (djk+djl)/pj - 1
    bot <- 4*(n-1)
    if( is.finite(bot) )
      return( top/bot )
    else
      return( NA )
  }
  
  .lynchrit <- function(dij,dik,dil,djk,djl,pi,pj,n){
    top <- pi*(djk+djl) + pj*(dik+dil) - 4*pi*pj
    bot <- (1+dij)*(pi+pj) - 4*pi*pj
    return( top/bot )
  }

  theFunc <- .rit96
  if( mode == "LynchRitland")
    theFunc <- .lynchrit
  
  
  N <- length( loci )
  ret <- matrix(0,nrow=N,ncol=N)
  for( i in 1:N){
    loc1 <- alleles(loci[i])
    if( length(loc1) == 2 ) {    
      dij <- ifelse(is_heterozygote(loci[i]), 1, 0 )
      pi <- freqs$Frequency[ freqs$Allele==loc1[1]]
      pj <- freqs$Frequency[ freqs$Allele==loc1[2]]
      for(j in 1:N){
        if(i!=j){
          loc2 <- alleles(loci[j])
          val <- NA
          if( length(loc2)==2 ) {
            dik <- ifelse( loc1[1]==loc2[1], 1, 0 )
            dil <- ifelse( loc1[1]==loc2[2], 1, 0 )
            djk <- ifelse( loc1[2]==loc2[1], 1, 0 )
            djl <- ifelse( loc1[2]==loc2[2], 1, 0 )
            val <- theFunc(dij,dik,dil,djk,djl,pi,pj,n)
          }
          ret[i,j] <- val
        }
      }
    }
  }

  return(ret)
}