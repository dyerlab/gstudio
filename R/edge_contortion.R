#' Determines of edges are stretched or compressed and adds attribute
#'  to popgraph object.
#' 
#' This function measure the deviation of edges in physical space from the expected 
#'   value given by the genetic distance.
#' @param graph A \code{popgraph} object
#' @param latitude The name of the node property that has the value of the latitude for
#'   that node (default 'Latitude').
#' @param longitude The name of the edge property that has the longitude (default 'Longitude')
#' @param P A non-genetic distance (physical or ecological) matrix on which the genetic distances 
#'   are compared.
#' @return A copy of the graph with new properties 'contortion' (the magnitude of differences
#'   between genetic and physical distances) and 'stretch' (as 'Compressed' or 'Extended')
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
edge_contortion <- function( graph, latitude="Latitude", longitude="Longitude", P=NULL){
  if( is.null(P))
    stop("You must supply a physical distance matrix with this function.")
  if( !is(graph,"popgraph"))
    stop("This function only works with Population Graph objects")
  
  atr <- vertex_attr_names(graph)
  if( !(all( c(longitude,latitude) %in% atr)))
    stop("You must specify latitude & longitude as node attributes.")
  
  G <- as.matrix( as_adjacency_matrix(graph,attr = "weight") )
  
  if( dim(G) != dim(P) )
    stop("Your physical and genetic distance matrices are not the same size.")
  
  G <- G / sum( G )
  G[ G==0 ] <- NA
  coords <- data.frame( Stratum=V(graph)$name,
                        Longitude=vertex_attr(graph,longitude),
                        Latitude=vertex_attr(graph,latitude))
  

  P[ is.na(G)] <- 0
  P <- P / sum(P)
  
  C <- G - P
  
  idx <- igraph::ends(graph, E(graph), names=FALSE)
  igraph::E(graph)$contortion <- 0
  igraph::E(graph)$stretch <- ""
  for( i in 1:nrow(idx) ) {
    val <- C[idx[i,1],idx[i,2]]
    igraph::E(graph)$contortion[i] <- val
    igraph::E(graph)$stretch[i] <- ifelse( igraph::E(graph)$contortion[i] > 0 , "Compressed", "Extended")
  }

  return( graph )
}