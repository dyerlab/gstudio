#' Returns multilocus diversity
#' 
#' This function returns the unique population size of the population
#'  if all individuals were treated as multilocus individuals.
#' @param x A data frame with \code{locus} objects in it.
#' @return The fraction of that data set that have unique multilocus genotypes.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
multilocus_diversity <- function( x ) {
  if( !is(x,'data.frame'))
    stop("You need to pass a data.frame to the multilocus_diversity() function.")
  if( !length(column_class(x,"locus")))
    stop("You need to provide a data.frame with at least a single locus in it...")
  df <- data.frame( ID=1, x[, column_class(x,"locus")])
  Nu <- nrow(unique( df ))
  N <- nrow(x)
  return( Nu/N )
}