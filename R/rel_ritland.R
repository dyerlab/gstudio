#' This estimates the relatedness statistic from Ritland (1996)
#' 
#' This function takes a \code{locus} vector or a \code{data.frame} of
#'  locus objects and produces a pairwise distance matrix of relatedness
#'  values after Ritland (1996).
#' @param x A \code{data.frame} or \code{vector} that has \code{locus} objects
#'  in it.  If you pass it a \code{data.frame} it will return the multilocus
#'  relatedness.
#' @return A matrix of relatedness statistics.
#' @export
#' @author Rodney J. dyer <rjdyer@@vcu.edu>
#' 
rel_ritland <- function( x ) {
  if( is(x,"locus"))
    x <- data.frame( Locus=x )
  
  if( !is(x,"data.frame"))
    stop("You need to pass either a locus or a data.frame to this function.")
  
  N <- nrow( x )
  ret <- matrix(0,N,N)
  loci <- column_class( x, "locus") 
  if( !length(loci) )
    stop("Cannot estimate relatedness with no loci...  Am I supposed to make up the data for you?")
  w <- 1
  
  for( locus_name in loci ){
    locus <- x[[locus_name]]
    r <- matrix(0,N,N)
    freq <- frequencies( locus )
    n <- nrow(freq)
    for( i in 1:N){
      a <- alleles( locus[i] )
      if( length(a)==2 ) {
        p1 <- freq$Frequency[ freq$Allele==a[1] ]
        p2 <- freq$Frequency[ freq$Allele==a[2] ]
        for( j in 1:N){
          if( i!=j){
            d <- kronecker_delta( locus[i], locus[j] )
            r[i,j]  <-  ( ((d[2]-d[3])/p1) + ((d[4]+d[5])/p2) -1 ) / (4*(n-1)) 
          }
        }        
      }
    }
    
    r <- (r + t(r))/2
    
    if( length(loci) ) {
      wt <- length(freq$Allele)-1
      r <- r*wt
      w <- w + (wt)
    }
    ret <- ret + r
  }
  
  ret <- ret / wt
  diag(ret) <- NA
  return(ret)
}