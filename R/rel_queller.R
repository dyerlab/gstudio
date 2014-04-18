#' This estimates the Queller & Goodnight (1989)
#' 
#' This function estimates the allele-wise relatedness statistic from
#'  Queller & Goodnight (1989). 
#'  
#' This is an assymetric estimator using the formula:
#'  
#'  \deqn{ r_{XY} = \frac{\sum_{i=1}^L(\delta_{ac} + \delta_{ad} + \delta_{bd}  + \delta_{bd} -p_a - p_b - p_c - p_d) }{\sum_{i=1}^L (2 + \delta_{ab} + \delta_{cd} - p_a - p_b - p_c - p_d)} }
#'  
#' @note For missing data and for data comparing heterozygotes at a 2-allele locus
#'  this estimator is undefined.  Moreover, I do add zero to both the numerator 
#' @param x A \code{data.frame} or \code{vector} that has \code{locus} objects
#'  in it.  If you pass it a \code{data.frame} it will return the multilocus
#'  relatedness.
#' @return A matrix of pairwise relatedness values.
#' @author Rodney J. dyer <rjdyer@@vcu.edu>
#' @export
#' 
rel_queller <- function( x ){
  if( is(x,"locus"))
    x <- data.frame( Locus=x )
  if( !is(x,"data.frame"))
    stop("You need to pass either a locus or a data.frame to this function.")
  
  N <- nrow( x )
  ret.top <- matrix(0,N,N)
  ret.bot <- matrix(0,N,N)
  
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
              ret.top[i,j] <- ret.top[i,j] + (d[2] + d[3] + d[4] + d[5] - pa - pb - pc - pd)
              ret.bot[i,j] <- ret.bot[i,j] + ( 2 + d[1] + d[6] - pa - pb - pc - pd )
            }
          }
        }        
      }
    }
  }
  
  
  # Do the multilocus stuff
  ret.top <- ret.top + t(ret.top)
  ret.bot <- ret.bot + t(ret.bot)
  
  diag(ret.bot) <- 1
  ret <- ret.top/ret.bot
  diag(ret) <- NA
  
  return( ret )
}
  