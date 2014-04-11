#' This estimates Fij allele-wise 
#' 
#' This function estimates the allele-wise coancestry statistic fij from
#'  Nason.  It can be used as a single locus or multilocus estimator if 
#'  you provide the correct standardizations.
#' @param x A vector of loci to use.
#' @param allele The allele to estimate.  If this is left blank, a locus-wide
#'  estimator is provided.  That is the average of the allele-wise estimators
#'  standardized by the polymorphic index, \code{Pe()}.
#' @return A matrix of pairwise values for either the allele or 
#' 
Fij <- function( x, allele=NA ){
  if( !is(x,"locus"))
    stop("This function takes a vector of locus class objects as an argument.")
  N <- length(x)
  k <- N*(N-1)/2
  ret <- matrix(0,N,N)
  diag(ret) <- 1
  
  freq <- frequencies( x )
  if( is.na(allele) ) 
    allele <- freq$Allele
  else 
    freq <- freq[ freq$Allele %in% allele, ]
  
  if( nrow(freq)==0 )
    stop("The allele you requested is not in this data set so there is no way to estimate coancestry.")
  
  loci <- to_mv(x,alleles=allele )
  
  pbar <- freq$Frequency
  
  for( i in 1:N){
    pi <- loci[i,]
    for( j in 1:i) {
      if( i!=j) {
        pj <- loci[j,]
        fij <- mean( ((pi-pbar)*(pj-pbar))/(k*pbar*(1-pbar)) + 1/(2*(N-1)) )   
        ret[i,j] <- ret[j,i] <- fij
      }
    }
  }
  
  # correct for multi allelic estimators
  if( length(allele) > 1 )
    ret <- ret / Pe(x)
  
  return(ret)
}