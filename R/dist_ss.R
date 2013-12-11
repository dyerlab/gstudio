#' Estimation of SS distance
#' 
#' This function returns a measure of genetic distance based upon
#'  the partitioned Sums of Squares from the AMOVA analysis.  There are 
#'  no particular assumptions to this estimation.
#' @param x A data frame with locus objects in it as well as a stratum object.
#' @param stratum A factor indicating the stratum to use.
#' @return The SS distance matrix
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
dist_ss <- function( x, stratum="Population") {
  
  if( !is(x,"data.frame"))
    stop("This only works on a data.frame")
  if( length( column_class(x,"locus"))==0 )
    stop("You need to have some loci in the data.frame to estimate ss distance.")
  
  D <- genetic_distance( x, mode="AMOVA" )
  labels <-  as.character(x[[stratum ]])
  pop_names <- unique( labels )
  K <- length( pop_names )
  ret <- matrix(0,nrow=K,ncol=K)
  N <- nrow(x)
  
  for( i in 1:K) {
    iidx <- labels==pop_names[i]
    ni <- sum( iidx )
    for ( j in i:K) {
      jidx <- labels==pop_names[j]
      nj <- sum(jidx)
      val <- sum(D[iidx,jidx]) / ( ni + nj )
      ret[i,j] <- val
      if( i!=j)
        ret[j,i] <- val
    }
  }
  rownames(ret) <- colnames(ret) <- labels
  return( ret )  
}