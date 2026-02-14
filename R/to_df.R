#' Converts popgraph into Data.Frame
#' 
#' Quick function to convert a population graph into a data.frame
#'   object for either the node or edge set with all attributes.
#' @param graph An population graph
#' @param mode Either "nodes" or "edges" to be used.
#' @return An object of type data.frame
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#'
to_df <- function( graph, mode=c("nodes","edges")[1]) {
  if( !inherits(graph,"popgraph"))
    stop("This function requires a population graph to work")

  if( !(mode %in% c("nodes","edges")))
    stop("unsupported mode")
  
  ret <- NULL
  
  if( mode == "nodes") {
    keys <- vertex_attr_names(graph)
    for( key in keys){
      ret[[key]] <- vertex_attr(graph,key)
    }
    ret <- as.data.frame(ret)
  }
  else if( mode == "edges") {
    elist <- as_edgelist(graph,names=TRUE)
    ret$From = elist[,1]
    ret$To = elist[,2]
    for( attr in edge_attr_names(graph) ) {
      ret[[attr]] <- edge_attr(graph,"weight")
    }  
  }
  
  return( as.data.frame(ret,stringsAsFactors=FALSE) )
}