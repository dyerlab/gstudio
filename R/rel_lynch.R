#' This estimates the Lynch & Ritland (1999) 
#' 
#' This function estimates the allele-wise relatedness statistic from
#'  Lynch & Ritland (1999). It is very sensitive to the presence of 
#'  rare alleles.
#'  
#' This is an symetric estimator using the formula:
#' 
#' \deqn{\hat{r}_{XY} = \frac{r_{XY} + r_{YX}}{2}}
#' 
#' where
#'  
#'  \deqn{r_{XY} = \frac{1}{\sum_{i=1}^L w_i} \sum_{i=1}^L \frac{p_a(\delta_{bc}+\delta_{bd}) + p_b(\delta_{ac} + \delta_{ad}) - 4p_a p_b}{2p_a p_b} }
#'  
#' and 
#' 
#' \deqn{w_i = \frac{((1+\delta_{ab})(p_a+p_b) - 4*p_a p_b)}{2p_a p_b}}
#'  
#' For missing genotypes, relatedness is not estimated for any pair-wise comparison. 
#'  This does not constitute an error, specifically, but you should be careful of 
#'  noting which comparisons are made from no or at least fewer than the full set
#'  of loci.
#' @note For missing data and for data comparing heterozygotes at a 2-allele locus
#'  this estimator is undefined.  
#' @param x A \code{data.frame} or \code{vector} that has \code{locus} objects
#'  in it.  If you pass it a \code{data.frame} it will return the multilocus
#'  relatedness.
#' @return A matrix of pairwise values
#' @export
#' @author Rodney J. dyer <rjdyer@@vcu.edu>
#' 
rel_lynch <- function( x ){
  if( is(x,"locus"))
    x <- data.frame( Locus=x )
  if( !is(x,"data.frame"))
    stop("You need to pass either a locus or a data.frame to this function.")
  
  N <- nrow( x )
  ret <- matrix(0,N,N)
  w <- matrix(0,N,N)
  
  loci <- column_class( x, "locus" )
  if( length(loci) < 1 )
    stop("You need to pass some loci to this function...")
  
  for( locus_name in loci ) {
    locus <- x[[locus_name]]
    freqs <- frequencies(locus)
    for( i in 1:N){
      a <- alleles(locus[i])
      
      if( length(a)==2 ){
        pa <- freqs$Frequency[ freqs$Allele==a[1] ]
        pb <- freqs$Frequency[ freqs$Allele==a[2] ]
        for( j in 1:N){
          if( i!=j ){
            a <- alleles( locus[j])
            if( length(a)==2){
              pc <- freqs$Frequency[ freqs$Allele==a[1] ]
              pd <- freqs$Frequency[ freqs$Allele==a[2] ]
              d <- kronecker_delta(locus[i],locus[j])
              ret[i,j] <- (pa*(d[4]+d[5]) + pb*(d[2]+d[3])-4*pa*pb)/(2*pa*pb)
              w[i,j] <- w[i,j] <- ((1+d[1])*(pa+pb)-4*pa*pb)/(2*pa*pb)
            }
          }
        }        
      }
    }
  }
  
  
  # Do the multilocus stuff
  ret <- ret * (1/w)
  ret <- (ret + t(ret)) / 2
  diag(ret) <- NA
  
  
  return( ret )
}
  