#' Converts graph to a pgraph file format
#' 
#' This is a simple function that takes the graph and
#'  converts it into a *.pgraph file for visualization 
#'  in other software.
#' @param graph An object of type \code{popgraph}
#' @param file The name and location of where the *.pgraph file is to be saved.
#'  If ommitted, this function will return a single text file.
#' @return Nothing if passed a file or the raw text of the *.pgraph file if
#'  you do not provide a file object.
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
to_pgraph <- function( graph, file ) {
  if( !is(graph,"popgraph") )
    stop("This function only works using a popgraph object.")

  K <- length(igraph::V(graph))
  L <- length(igraph::E(graph))
  
  if( !("name" %in% vertex_attr_names(graph)))
    igraph::V(graph)$name <- paste("Node",1:K,sep="-")
  if( !("size" %in% vertex_attr_names(graph)))
    igraph::V(graph)$size <- 1
  if( !("color" %in% vertex_attr_names(graph)))
    igraph::V(graph)$color <- 1
  
  if( !("weight" %in% edge_attr_names(graph) ) )
    igraph::E(graph)$weight <- 1
  
  pgraph_text <- paste( K, "\t", L, sep="")
  
  for( i in 1:K )
    pgraph_text <- append( pgraph_text, paste( igraph::V(graph)$name[i], "\t", 
                                               igraph::V(graph)$size[i], "\t", 
                                               igraph::V(graph)$color[i], sep="") )
  
  A <- as_adjacency_matrix(graph,attr="weight")
  for( i in 1:K )
    for( j in i:K)
      if( A[i,j] > 0 )
        pgraph_text <- append( pgraph_text, paste( igraph::V(graph)$name[i],"\t",
                                                   igraph::V(graph)$name[j],"\t",A[i,j],sep="") )
  
  pgraph_text <- paste( pgraph_text, collapse="\n")
  
  
                               
                               
  if( !missing(file) ) {
    write(pgraph_text,file)
    invisible(pgraph_text)
  }
  else
    return( pgraph_text )
}

