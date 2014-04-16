#' This estimates the Queller & Goodnight (1989)
#' 
#' This function estimates the allele-wise relatedness statistic from
#'  Queller & Goodnight (1989). 
#' @note For missing data and for data comparing heterozygotes at a 2-allele locus
#'  this estimator is undefined.
#' @param x Either a 
#' @return A matrix of pairwise values for either the allele or 
#' 
rel_queller <- function( x, allele=NA ){
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
  
  for( locus in loci ) {

    freqs <- frequencies(x[[locus]])
    print(locus)
    print(freqs)
    
    for( i in 1:N) {
      loc1 <- alleles(x[[locus]][i])
      pi <- freqs$Frequency[ freqs$Allele == loc1[1] ]
      pj <- freqs$Frequency[ freqs$Allele == loc1[2] ]
      for( j in 1:N) {
          d <- kronecker_delta( x[[locus]][i], x[[locus]][j] )
          cat(c(x[[locus]][i],x[[locus]][j]),"\n")
          print(d)
          ret.top[i,j] <- ret.top[j,i] <- 0.5*(d[2]+d[5]+d[4]+d[3]) - pi - pj
          ret.bot[i,j] <- ret.bot[j,i] <- 1 + d[1] - pi - pj          
      }
    }
    print(ret.top)
    print(ret.bot)
    
  }
  
  
  # Do the multilocus stuff
  ret <- ret.top/ret.bot

  return( ret )
}
  