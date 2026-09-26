#' Asymmetric Population Graph
#'
#' A convenience wrapper around \code{\link{asymmetric_weights}} that converts
#' an undirected \code{popgraph} into a directed one.  Each undirected edge
#' \eqn{(i,j)} becomes two directed edges whose weights (\code{dij} and
#' \code{dji}) partition the original conditional genetic distance
#' proportionally to the local Markov flow probabilities, so the total graph
#' distance is preserved.
#'
#' @param graph An undirected \code{popgraph} object.
#' @param perplexity Target perplexity for the per-node bandwidth search (default 4).
#' @param tol Convergence tolerance for the binary search on sigma (default 1e-5).
#' @param max_iter Maximum iterations for the sigma binary search (default 100).
#' @return A directed \code{popgraph} object with edge attribute \code{weight}
#'   set to the directional partial distance (\code{dij} or \code{dji}).
#' @seealso \code{\link{asymmetric_weights}}
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
#' @examples
#' \donttest{
#' data(arapat)
#' mv <- to_mv(arapat)
#' groups <- arapat$Population
#' graph <- popgraph(mv, groups)
#' asymmetric_popgraph(graph)
#' }
asymmetric_popgraph <- function(graph, perplexity = 4, tol = 1e-5, max_iter = 100) {
  if (!igraph::is_igraph(graph)) stop("Input must be an igraph object")
  if (igraph::is_directed(graph)) stop("Input graph must be undirected")

  w <- asymmetric_weights(graph, perplexity = perplexity, tol = tol, max_iter = max_iter)

  # Start from the full vertex set (in the original order) so isolated nodes
  # are retained, and carry over all vertex attributes (e.g. coordinates, labels)
  g_dir <- igraph::make_empty_graph(n = igraph::vcount(graph), directed = TRUE)
  for (attr in igraph::vertex_attr_names(graph)) {
    igraph::vertex_attr(g_dir, attr) <- igraph::vertex_attr(graph, attr)
  }

  # Each undirected edge becomes two directed edges
  if (!is.null(w) && nrow(w) > 0) {
    edge_mat <- rbind(
      cbind(as.character(w$i), as.character(w$j)),
      cbind(as.character(w$j), as.character(w$i))
    )
    g_dir <- igraph::add_edges(g_dir, as.vector(t(edge_mat)),
                               weight = c(w$dij, w$dji))
  }

  return(as.popgraph(g_dir))
}
