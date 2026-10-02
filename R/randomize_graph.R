#' Randomize Graph
#' 
#' This function randomizes the edges in a popgraph and 
#'  returns a new graph.
#' @param graph An object of type \code{popgraph} or \code{igraph}
#' @param mode The kind of randomization to conduct, can be "full"
#'  which makes a new graph with the same number of edges as the 
#'  original one, or "degree" which preserves the degree sequence
#'  of the original graph by repeated degree-preserving edge swaps
#'  (\code{\link[igraph]{rewire}} with \code{\link[igraph]{keeping_degseq}}),
#'  never creating self-loops or multiple edges.
#' @return An \code{igraph} object with randomized edges.
#' @export
#' @examples
#' data(lopho)
#' g_rand <- randomize_graph(lopho, mode = "degree")
#' g_rand

randomize_graph <- function( graph=NULL, mode=c("full","degree")[2] ) {
  
  if( is.null(graph))
    stop("Cannot run without a network")

  
  if( mode == "full"){
    e <- igraph::as_adjacency_matrix(graph,sparse = FALSE)
    vals <- e[ lower.tri(e)]
    new_vals <- sample( vals, size=length(vals), replace=FALSE) 
    a <- matrix(0, nrow=nrow(e), ncol=ncol(e))
    a[ lower.tri(a)] <- new_vals
    a <- a + t(a)
    g <- igraph::graph_from_adjacency_matrix(a,mode = "undirected" )
    return( g )
  } 
  else if( mode == "degree" ){
    # Ten swap attempts per edge mixes the edge set well away from the
    # original; swaps that would create a loop or multi-edge are rejected
    # individually rather than discarding the whole configuration.
    niter <- 10L * igraph::ecount(graph)
    g <- igraph::rewire(graph, with = igraph::keeping_degseq(loops = FALSE,
                                                             niter = niter))

    # Swapped edges carry arbitrary attributes, so return a bare topology.
    for (a in igraph::edge_attr_names(g))
      g <- igraph::delete_edge_attr(g, a)
    class(g) <- "igraph"
    return(g)
  }
  
  stop("Unknown mode to randomize_graph")

}
