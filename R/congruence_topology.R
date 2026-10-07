#' Returns congruence topology
#' 
#' Takes two graphs and returns topology that is 
#'  the intersection of the edge sets.
#' @param graph1 An object of type \code{popgraph}
#' @param graph2 An object of type \code{popgraph}
#' @param warn.nonoverlap A flag indicating that a warning should be thrown
#'  if the node sets are not equal (default = TRUE )
#' @return An object of type \code{popgraph} where the node and edge 
#'  sets are the intersection of the two.  Vertex attributes of the parent
#'  graphs (e.g. \code{Longitude}/\code{Latitude} from
#'  \code{population_graph(decorate = TRUE)} or \code{\link{decorate_graph}})
#'  are carried over: an attribute found in only one graph is copied, one found
#'  in both with the same values on the shared nodes is copied once, and one
#'  whose values differ (such as \code{size}, each graph's own within-population
#'  variability) is kept from both as \code{<name>.1} and \code{<name>.2}.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
#' @examples
#' data(lopho)
#' g_cong <- congruence_topology(lopho, lopho)
#' g_cong
congruence_topology <- function( graph1, graph2, warn.nonoverlap=TRUE ) {
  
  if( !inherits(graph1, "popgraph") | !inherits(graph2, "igraph") )
    stop("congruence.topology() requires that you pass an igraph or popgraph object.")
  
  if( warn.nonoverlap & length(setdiff( V(graph1)$name, V(graph2)$name )))
    warning("These two topologies have non-overlapping node sets!  Careful on interpretation.")
  
  nodes <- igraph::V(graph1)$name
  for( i in 1:length(nodes)){
    if( !(nodes[i] %in% igraph::V(graph2)$name))
      nodes[i] <- NA
  }
  cong.nodes <- nodes[ !is.na(nodes) ]
  #cong.nodes <- intersect( V(graph1)$name, V(graph2)$name )
  a <- as.matrix( as_adjacency_matrix( induced_subgraph( graph1, cong.nodes ) ) )
  nms <- row.names(a)
  b <- as.matrix( as_adjacency_matrix( induced_subgraph( graph2, cong.nodes ) ) )
  b <- b[nms,nms]
  
  #a <- as.matrix( as_adjacency_matrix(graph1))
  #b <- as.matrix( as_adjacency_matrix(graph2))
  
  cong <- graph_from_adjacency_matrix( a*b, mode="undirected" )
  cong <- .congruence_vertex_attrs( cong, graph1, graph2 )
  class(cong) <- c("popgraph", "igraph")
  return( cong )

}


# Copy the parents' vertex attributes onto the congruence topology (see the
# @return section of congruence_topology()).
#' @keywords internal
#' @noRd
.congruence_vertex_attrs <- function( cong, graph1, graph2 ) {
  nodes <- igraph::V(cong)$name
  pull <- function( g ) {
    idx <- match( nodes, igraph::V(g)$name )
    va <- igraph::vertex_attr(g)
    lapply( va[ setdiff(names(va), "name") ], function(v) as.vector(v)[idx] )
  }
  a1 <- pull(graph1)
  a2 <- pull(graph2)
  for( att in union(names(a1), names(a2)) ) {
    v1 <- a1[[att]]
    v2 <- a2[[att]]
    if( is.null(v2) || (!is.null(v1) && isTRUE(all.equal(v1, v2, check.attributes = FALSE))) )
      cong <- igraph::set_vertex_attr( cong, att, value = v1 )
    else if( is.null(v1) )
      cong <- igraph::set_vertex_attr( cong, att, value = v2 )
    else {
      cong <- igraph::set_vertex_attr( cong, paste0(att, ".1"), value = v1 )
      cong <- igraph::set_vertex_attr( cong, paste0(att, ".2"), value = v2 )
    }
  }
  cong
}
