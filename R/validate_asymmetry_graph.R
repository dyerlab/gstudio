# Internal: shared input validation for the asymmetry inference functions.
#' @importFrom igraph is_igraph is_directed E
#' @keywords internal
#' @noRd
.validate_asymmetry_graph <- function(graph) {
  if (!igraph::is_igraph(graph))
    stop("'graph' must be an igraph/popgraph object")
  if (igraph::is_directed(graph))
    stop("'graph' must be undirected")
  if (is.null(igraph::E(graph)$weight))
    stop("'graph' must have a numeric 'weight' edge attribute")
  invisible(TRUE)
}
