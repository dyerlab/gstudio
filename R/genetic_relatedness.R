#' Estimates pair-wise relatedness
#' 
#' This is the primary front-end function for estimating pairwise genetic relatedness
#'  among individuals.
#' @param x A \code{data.frame} with \code{locus} columns, or a single \code{locus} vector.
#' @param loci The loci to use (if omitted, all loci are used).
#' @param mode The relatedness metric to use:
#'    \describe{
#'      \item{Nason}{Fij estimator of coancestry (Nason)}
#'      \item{LynchRitland}{Lynch & Ritland (1999) regression estimator}
#'      \item{Ritland}{Ritland (1996) estimator}
#'      \item{Queller}{Queller & Goodnight (1989) allele-wise relatedness}
#'    }
#' @param freqs An optional \code{data.frame} of allele frequencies as returned by \code{\link{frequencies}}.
#'  If omitted, estimated from the data.
#' @return A matrix of pairwise relatedness estimates.
#' @note This function operates on diploid data and will return NA for any 
#'  comparison of missing genotypes.
#' @export
#' @examples
#' loc1 <- c( locus(c("A","A")), locus(c("A","B")), locus(c("B","B")) )
#' loc2 <- c( locus(c("1","1")), locus(c("1","2")), locus(c("2","2")) )
#' df <- data.frame( TPI=loc1, PGM=loc2 )
#' genetic_relatedness( df, mode="Nason" )
#' genetic_relatedness( df, mode="LynchRitland" )
#' genetic_relatedness( df, mode="Ritland" )
#' genetic_relatedness( df, mode="Queller" )

genetic_relatedness  <- function( x, loci=NA, mode=c("Nason","LynchRitland","Ritland","Queller")[1],freqs=NA ) {
  
  if( is(x,"locus"))
    x <- data.frame(x)
  if( !is(x,"data.frame"))
    stop("Cannot perform relatedness estimates on data that is not either a data.frame or a locus vector.")
  if( !length(column_class(x,"locus")) )
    stop("You need to have genetic loci in the data.frame to estimate relatedness")
  if( is.na(all(freqs)) )
    freqs <- frequencies( x )
  
  if( length(loci) == 1 && is.na(loci) )
    loci <- column_class(x,"locus")
  N <- nrow(x)
  
  ret <- matrix(0,nrow=N,ncol=N)
  
  mode_clean <- tolower(mode)
  
  if( mode_clean %in% c("nason", "fij") ){
    for( locus_name in loci)
      ret <- ret + rel_nason(x[[locus_name]])
    ret <- ret * 1/length(loci)
  }
  else if( mode_clean %in% c("lynchritland", "lynch") ){
    ret <- rel_lynch( x[, loci, drop=FALSE] )
  }
  else if( mode_clean == "ritland" ){
    ret <- rel_ritland( x[, loci, drop=FALSE] )
  }
  else if( mode_clean %in% c("queller", "quellergoodnight") ){
    ret <- rel_queller( x[, loci, drop=FALSE] )
  }
  else
    stop("Unrecognized relatedness statistic requested: ", mode)
  
  
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






