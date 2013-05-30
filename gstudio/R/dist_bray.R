#' Estimation of jaccard distance
#' 
#' This function returns a measure of genetic distance based upon
#'  the AMOVA distance metric.  
#' @param x A \code{data.frame} with both stratum and \code{locus} 
#'  objects in them.
#' @param stratum The name of the stratum variable in \code{x}
#' @return A matrix of Jaccard distance
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
dist_bray <- function( x, stratum="Population" ) {
  
  if( !is( x, "data.frame") )
    stop("You need to pass a data.frame to dist_bray() to work.")
  
  if( !(stratum %in% names(x)))
    stop("You need to specify the correct stratum for dist_bray() to work.")
  
  locus_names <- column_class( x, "locus")
  K <- length( locus_names )
  if( K==0)
    stop("You need to pass objects of type 'locus' to use for dist_bray().")
  else if( K > 1 )
    warning("Jaccard distance will be assumed to be entirely additive across loci.")
 
  j <- dist_jaccard( x, stratum )
  ret <- -j / (j-2)
  return(ret)
}
