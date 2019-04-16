#' Convience function to create population graph object from genotypes.
#' 
#' This function is a convience function that wraps together the subset_loci() (potentially)
#'   function with the popgraph::popgraph() function (and translations from genotypes to 
#'   multivariate data).
#' @param x A \code{data.frame} with \code{locus} objects
#' @param stratum The stratum column to use as the Node designation (default="Population").
#' @param numLoci If not \code{NULL} then how many randomly selected loci (fewer than the total)
#'   to use in the estimation.
#' @param ... Other parameters passed to the \code{popgraph::popgraph()} function.
#' @export

population_graph <- function( x, stratum="Population", numLoci=NULL, ...) {
  if( !is(x,"data.frame")){
    stop("Must pass a data.frame object to this function.")
  }
  if( !(stratum %in% names(x))) {
    stop("You provided an invalid (or non-existent) column to be used as strata.")
  }
  
  
  if( is.null(numLoci) ) {
    data <- to_mv(x)
  } else {
    df <- subsample_loci(x, numLoci=numLoci)
    data <- to_mv( df )
  }

  groups <- as.factor( x[[stratum]] )
  
  return( popgraph::popgraph(data,groups,...))
}