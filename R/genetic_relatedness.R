#' Estimates pair-wise relatedness
#' 
#' This function returns single relatedness estimates as a 
#'  pairwise matrix.
#' @param x A \code{data.frame} that has \code{locus} columns.
#' @param loci The loci to use (if missing all loci are used).
#' @param mode The kind of relatedness to be estimated.  Currently
#'  Fij (the default) and LynchRitland are available.
#' @param freqs An optional \code{data.frame} (as returned by the
#'  function \code{frequencies()} with allele frequencies).  If this 
#'  is not provided, it will be estimated from all the data.  This allows
#'  you to estimate relatedness among subsets of individuals using 
#'  more global measures of relatedness.
#' @return A matrix of pairwise relatedness estimates.
#' @note This only works on diploid data and will return NA for any 
#'  comparison of missing genotypes.
#' @export
#' @examples
#' loci <- c( locus(1:2), locus(c(2,2)), locus(1:2) )
#' genetic_relatedness( loci )
#' genetic_relatedness( loci, freqs = data.frame( Allele=c("1","2"), Frequency=c(0.5,0.5)))


genetic_relatedness  <- function( x, loci=NA, mode=c("Nason","LynchRitland")[1],freqs=NA ) {
  
  if( is(x,"locus"))
    x <- data.frame(x)
  if( !is(x,"data.frame"))
    stop("Cannot perform relatedness estimates on data that is not either a data.frame or a locus vector.")
  if( any(is.na(column_class(x,"locus"))) )
    stop("You need to have genetic loci in the data.frame to estimate relatedness")
  if( is.na(freqs) )
    freqs <- frequencies( x )
  
  if( is.na(loci) )
    loci <- column_class(x,"locus")
  N <- nrow(x)
  
  ret <- matrix(0,nrow=N,ncol=N)
  
  
  if( mode=="Nason"){
    for( locus_name in loci)
      ret <- ret + rel_nason(x[[locus_name]])
    ret <- ret * 1/length(loci)
  }
  else if( mode=="LynchRitland" || mode=="Ritland"){
    for( locus in loci ){
      freq <- frequencies( x[[locus]] )
      val <- .relatedness_kronecker( x[[locus]], freq, length(loci)>1,mode )
    }
  }
  else
    stop("Unrecognized relatedness statistic requested")
  
  
  diag(ret) <- 1
  
  return( ret )
}

.relatedness_kronecker <- function( loci, freq, correctMultilocus, mode ){
  if( mode == "LynchRitland" ) {
    theFunc <- function(dij,dik,dil,djk,djl,pi,pj,n){
      top <- pi*(djk+djl) + pj*(dik+dil) - 4*pi*pj
      bot <- (1+dij)*(pi+pj) - 4*pi*pj
      return( top/bot )
    }
    
    theCorrection <- function(pi,pj,dij,n){
      top <- 2*pi*pj 
      bot <- (1+dij)*(pi+pj) - 4*pi*pj
      return( bot/top )
    }
    
    if( nrow(freq) < 6 ) 
      warning( "You should probably not use Lynch & Ritland estimator for loci with fewer than 6 alleles...")
  }
  
  else if( mode == "Ritland"){
    theFunc <- function(dij,dik,dil,djk,djl,pi,pj,n){
      top <- (dik+dil)/pi + (djk+djl)/pj - 1
      bot <- 4*(n-1)
      if( is.finite(bot) )
        return( top/bot )
      else
        return( NA )
    }
    
    theCorrection <- function(pi,pj,dij,n){
      return( n-1 )
    }
    
  }  
  
  n <- length( freq$Allele )
  N <- length( loci )
  ret <- matrix(1,nrow=N,ncol=N)
  for( i in 1:N){
    loc1 <- alleles(loci[i])
    if( length(loc1) == 2 ) {    
      dij <- ifelse(is_heterozygote(loci[i]), 1, 0 )
      pi <- freq$Frequency[ freq$Allele==loc1[1]]
      pj <- freq$Frequency[ freq$Allele==loc1[2]]
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
          
          if( correctMultilocus )
            val <- val* theCorrection(pi,pj,dij,n)
          
          ret[i,j] <- val
        }
      }
    }
  }
  
  return(ret)
  
}






