#' Randomize Graph
#' 
#' This function randomizes the edges in a popgraph and 
#'  returns a new graph.
#' @param graph An object of type \code{popgraph} or \code{igraph}
#' @param mode The kind of randomization to conduct, can be "full"
#'  which makes a new graph with the same number of edges as the 
#'  original one, or "degree" which preserves the degree distribution
#'  of the 
#' @export

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
    e <- igraph::as_edgelist(graph)
    
    v1 <- sort( e[,1] )
    v2 <- sample( e[,2], size=length(v1), replace=FALSE)
    
    new_edges <- cbind( v1, v2 )
    ctr <- 0 
    while( any(duplicated(new_edges)) || any( new_edges[,1] == new_edges[,2]) ) {
      new_edges <- cbind( v1, sample( v2, size=length(v2), replace=FALSE) )  
      ctr <- ctr + 1
      if( ctr > 10000 ) {
        stop("Error: Over 100 iterations for finding permutations without duplication or self-loops.  This may not be a real enough graph to do this routine.")
      }
    }
    
    if( any( duplicated(new_edges))){
      print(cbind( new_edges,duplicated(new_edges)))
      stop("Problem reaching convergence, try again.")
    }
    
    g <- igraph::graph_from_edgelist( new_edges, directed=FALSE )
    return(g)
  }
  
  stop("Unknown mode to randomize_graph")

}



