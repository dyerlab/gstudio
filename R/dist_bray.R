#' Estimation of jaccard distance
#' 
#' This function returns a measure of genetic distance based upon
#'  the AMOVA distance metric.  
#' @param x Either a \code{data.frame} with both stratum and \code{locus} 
#'  objects in them (for strata distance) OR a vector of \code{locus} 
#'  objects and this will calculate distance based upon individual
#'  genetic distances.
#' @param stratum The name of the stratum variable in \code{x}
#' @return A matrix of Jaccard distance
#' 
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples  
#' AA <- locus( c("A","A") )
#' AB <- locus( c("A","B") )
#' AC <- locus( c("A","C") )
#' BB <- locus( c("B","B") )
#' BC <- locus( c("B","C") )
#' CC <- locus( c("C","C") )
#' loci <- c(AA,AA,AB,AA,BB,BC,CC,BB,BB,CC)
#' df <- data.frame( Population=c(rep("A",5),rep("B",5) ), TPI=loci )
#' D <- dist_bray(df)
dist_bray <- function( x, stratum="Population" ) {
  
  # Special case where x is passed a a locus object, this 
  #   will assume that we are talking about individual Bray Curtis distance
  if( is(x,"locus") ) {
    x <- data.frame(Locus=x,Population=1:length(x))
    stratum <- "Population"
  }
  
  if( !is( x, "data.frame") )
    stop("You need to pass a data.frame to dist_bray() to work.")
  
  if( !(stratum %in% names(x)))
    stop("You need to specify the correct stratum for dist_bray() to work.")
  
  locus_names <- column_class( x, "locus")
  K <- length( locus_names )
  if( K==0)
    stop("You need to pass objects of type 'locus' to use for dist_bray().")
  else if( K > 1 )
    message("Bray distance will be assumed to be entirely additive across loci.")
 
  j <- dist_jaccard( x, stratum )
  ret <- -j / (j-2)
  return(ret)
}
