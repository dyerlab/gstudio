#' Returns distance congruence between two graphs
#'
#' Computes the shortest-path distance matrices for both graphs and returns the
#' result of a Pearson correlation test between pairwise distances. Only nodes
#' present in both graphs are used.
#'
#' @param graph1 An object of type \code{igraph} or \code{popgraph}.
#' @param graph2 An object of type \code{igraph} or \code{popgraph}.
#' @return An \code{htest} object as returned by \code{cor.test()}.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
test_congruence <- function(graph1, graph2) {
  cong.nodes <- intersect(igraph::V(graph1)$name, igraph::V(graph2)$name)

  if (is.null(cong.nodes) || length(cong.nodes) == 0)
    stop("There appear to be no nodes in common between these two graphs")

  A <- induced_subgraph(graph1, vids = cong.nodes)
  B <- induced_subgraph(graph2, vids = cong.nodes)

  Adis <- distances(A)[cong.nodes, cong.nodes]
  Bdis <- distances(B)[cong.nodes, cong.nodes]

  d1 <- Adis[lower.tri(Adis)]
  d2 <- Bdis[lower.tri(Bdis)]

  inf1 <- is.infinite(d1)
  inf2 <- is.infinite(d2)
  d1[inf1 | inf2] <- NA
  d2[inf1 | inf2] <- NA

  cor.test(d1, d2, na.action = na.omit)
}
