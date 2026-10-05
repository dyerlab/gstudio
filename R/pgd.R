#' Partitioned conditional genetic distance (pGD)
#'
#' @description
#' Splits each edge's conditional genetic distance \eqn{e_{ij}} into two
#' directional components in proportion to the neighbourhood weights,
#' \eqn{p_{ij} = e_{ij}\, w_{j|i} / (w_{j|i} + w_{i|j})} and
#' \eqn{p_{ji} = e_{ij}\, w_{i|j} / (w_{j|i} + w_{i|j})}, so that
#' \eqn{p_{ij} + p_{ji} = e_{ij}}.  Because pGD is a distance it is shorter in the
#' direction of gene flow: when \eqn{i} is a source for \eqn{j}, \eqn{p_{ij} < p_{ji}}.
#'
#' @param graph An undirected \code{popgraph}/\code{igraph} with a numeric
#'   \code{weight} edge attribute.
#' @param gamma Bandwidth multiplier (default \code{0.5}); see \code{\link{source_sink_scores}}.
#' @param output \code{"graph"} (a directed igraph whose arc weights are pGD) or
#'   \code{"matrix"} (all-pairs directed shortest-path pGD; \code{[i, j]} is the
#'   distance from \eqn{i} to \eqn{j}).
#' @return A directed \code{igraph} or a numeric matrix.
#' @seealso \code{\link{ibgd}}, \code{\link{genetic_distance}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' library(igraph)
#' A <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
#' A["A", "B"] <- A["B", "A"] <- 1; A["B", "C"] <- A["C", "B"] <- 2
#' A["C", "D"] <- A["D", "C"] <- 1.5; A["A", "C"] <- A["C", "A"] <- 2.5
#' g <- graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE)
#' pgd(g, output = "matrix")
#' @export
pgd <- function(graph, gamma = 0.5, output = c("graph", "matrix")) {
  output <- match.arg(output)
  graph <- .gravity_check(graph)
  nodes <- igraph::V(graph)$name
  W <- neighbourhood_weights(graph, gamma)
  el <- igraph::as_edgelist(graph, names = TRUE); e <- igraph::E(graph)$weight
  wa <- W[el]; wt <- W[el[, 2:1, drop = FALSE]]                     # w_{v|u}, w_{u|v}
  fr <- ifelse(wa + wt > 0, wa / (wa + wt), 0.5)
  dg <- igraph::graph_from_data_frame(data.frame(from = c(el[, 1], el[, 2]), to = c(el[, 2], el[, 1]),
                                                 weight = c(e * fr, e * (1 - fr))),
                                      directed = TRUE, vertices = data.frame(name = nodes))
  if (output == "graph") return(dg)
  igraph::distances(dg, v = nodes, to = nodes, mode = "out")
}
