# Internal: input check shared by the genetic-gravity functions.

#' @keywords internal
.gravity_check <- function(graph) {
  if (!igraph::is_igraph(graph)) stop("'graph' must be an igraph object")
  if (igraph::is_directed(graph)) stop("'graph' must be undirected")
  if (is.null(igraph::E(graph)$weight)) stop("'graph' must have a numeric 'weight' edge attribute")
  if (is.null(igraph::V(graph)$name)) igraph::V(graph)$name <- as.character(seq_len(igraph::vcount(graph)))
  graph
}
