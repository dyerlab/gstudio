#' Estimation of conditional genetic distance
#' 
#' This function returns a measure of conditional genetic distance based upon
#'  the Population Graphs approach from Dyer & Nason (2004) and Dyer et al. (2010).
#' @param stratum The groups among which you are going to estimate genetic distances.
#' @param x The genetic data, either as a single locus or multilocus (\code{data.frame}) 
#'  object.  
#' @return A matrix of conditional genetic distance estimates.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
dist_cgd <- function( x, stratum="Population" ) {
  require(popgraph)
  
  if( !is( x, "data.frame") )
    stop("You need to pass a data.frame to dist_cavalli() to work.")
  
  if( !(stratum %in% names(x)))
    stop("You need to specify the correct stratum for dist_cavalli() to work.")


  mv <- to_mv( x )
  graph <- popgraph:::population_graph(x=mv, groups=factor( as.character( x[[stratum]] )))
  ret <- popgraph:::to_matrix(graph, mode = "shortest path")
  return(ret)
}

