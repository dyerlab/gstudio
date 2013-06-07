#' Estimation of euclidean distance
#' 
#' This function returns a measure of genetic distance based upon
#'  the euclidean frequency distance metric.  
#' @param x The genetic data, either as a single locus or multilocus (\code{data.frame}) 
#'  object.  
#' @param stratum The groups among which you are going to estimate genetic distances.
#' @return A matrix of euclidean distance estimates.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @importFrom reshape2 dcast 
#' @examples
#'   AA <- locus( c("A","A") )
#'   AB <- locus( c("A","B") )
#'   BB <- locus( c("B","B") )
#'   loci <- c(AA,AA,AB,AA,BB,BB,BB,AB,BB,AB)
#'   df <- data.frame( Population=c(rep("A",5),rep("B",5) ), TPI=loci )
#'   dist_euclidean(df)
dist_euclidean <- function( x, stratum="Population" ) {
  
  if( !is( x, "data.frame") )
    stop("You need to pass a data.frame to dist_euclidean() to work.")
  
  if( !(stratum %in% names(x)))
    stop("You need to specify the correct stratum for dist_euclidean() to work.")
  
  locus_names <- column_class( x, "locus")
  K <- length( locus_names )
  if( K==0)
    stop("You need to pass objects of type 'locus' to use for dist_euclidean().")
  if( K > 1 )
    message("Multilous estimates of Euclidean distance are assumed to be additive.")

  freqs <- frequencies( x, stratum=stratum) 
  f <- dcast( freqs, Locus + Allele ~ Stratum, value.var="Frequency", fill=0)
  strata_names <- names(f)[3:length(names(f))]
  K <- length(strata_names)
  ret <- matrix( 0, K, K )
  colnames(ret) <- rownames(ret) <- strata_names
  
  for( i in 1:K ) {
    px <- f[, (i+2)]
    for( j in 1:K){
      py <- f[,(j+2)]
      d <- sqrt( sum(  (px-py)^2 ) )      
      ret[i,j] <- ret[j,i] <- d  
    }
  }
  
  return(ret)
}
