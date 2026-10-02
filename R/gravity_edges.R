#' @rdname gravity
#' @export
gravity_edges <- function(graph, gamma = 0.5, x = NULL) {
  graph <- .gravity_check(graph)
  nodes <- igraph::V(graph)$name
  W <- neighbourhood_weights(graph, gamma)
  el <- igraph::as_edgelist(graph, names = TRUE)
  wt <- igraph::E(graph)$weight
  xv <- .gravity_x(x, nodes)
  if (!is.null(xv)) {
    xi <- setNames(xv, nodes)
    flip <- xi[el[, 1]] > xi[el[, 2]]
    el[flip, ] <- el[flip, 2:1]
  }
  k <- setNames(igraph::degree(graph), nodes)
  w_from <- W[el[, c(2, 1), drop = FALSE]]                    # from's weight in to's neighbourhood
  w_to   <- W[el]                                             # to's weight in from's neighbourhood
  data.frame(from = el[, 1], to = el[, 2], weight = wt, w_from = w_from, w_to = w_to,
             Delta = w_from - w_to, Delta0 = 1 / k[el[, 2]] - 1 / k[el[, 1]],
             row.names = NULL, stringsAsFactors = FALSE)
}
