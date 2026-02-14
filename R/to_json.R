#' Converts to json representation
#' 
#' This is a nice function that takes a graph structure
#'  and converts it to a json format for use on the web.
#' @param graph An object of type \code{igraph}
#' @param file A file to write the output to (default missing)
#' @param forJS A flag to indicate putting a 'var myjson = ' part 
#'  the file.
#' @return A textual json representation of the graph
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
to_json <- function( graph, file, forJS=FALSE ) {
  
  if( !inherits(graph,"popgraph"))
    stop("This function requires a popgraph object to function")
  
  # make vector 
  quotify <- function( df ){
    keys <- names(df)
    ret <- "["
    cols <- ncol(df)
    for(i in 1:nrow(df) ) {
      row <- "{"
      for( j in 1:ncol(df)){
        row <- paste(row,"\"",keys[j],"\":",sep="")
        valsep <- ifelse( is.numeric( df[i,j]), "", "\"" )
        row <- paste(row,valsep,df[i,j],valsep,sep="")
        if( j < ncol(df) )
          row <- paste( row, ",", sep="")
      }
      ret <- paste(ret, row,"}",sep="")
      
      if( i < nrow(df))
        ret <- paste(ret,", ",sep="")
      
    }
    ret <- paste( ret, ']',sep="")
    return(ret)
  }
  
  
  # do the nodes
  node.attr.names <- vertex_attr_names( graph )
  if( !("name" %in% node.attr.names) )
    stop("Vertices are indexed by the property 'name' and your graph does not have one...")
  nodes <- data.frame( name=rep("libby",length(igraph::V(graph))) )
  for( attr in node.attr.names )
    nodes[[attr]] <- vertex_attr( graph, attr )
  if( !("group" %in% names(nodes) ) )
    nodes$group <- "All"
  nodestr <- quotify(nodes)
  
  # make the edges
  K <- length(E(graph))
  edgedf <- data.frame( source=rep(1,K), target=rep(1,K), weight=1)
  if( !("weight" %in% edge_attr_names(graph)))
    graph <- set_edge_attr(graph,"weight", value=1)
  wts <- as.matrix(as_adjacency_matrix(graph, attr="weight"))
  idx <- 1
  N <- length(igraph::V(graph))
  for( i in 1:N){
    for( j in i:N) {
      if(wts[i,j] > 0 ) {
        edgedf$source[idx] <- (i-1)
        edgedf$target[idx] <- (j-1)
        edgedf$weight[idx] <- wts[i,j]
        idx <- idx + 1
      }
    }
  }
  edgestr <- quotify(edgedf)
  
  
  if( forJS )
    ret <- paste("var myjson = '{ \"nodes\":", nodestr, ", \"links\":",edgestr,"}';", sep="")
  else 
    ret <- paste("{\n \"nodes\":", nodestr, ", \"links\":",edgestr,"\n}", sep="")
    
  if( !missing(file) )
    write(ret,file=file)
  else
    return( ret )
}