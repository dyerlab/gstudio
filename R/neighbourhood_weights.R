#' @rdname gravity
#' @export
neighbourhood_weights <- function(graph, gamma = 0.5) {
  graph <- .gravity_check(graph)
  if (!is.numeric(gamma) || length(gamma) != 1 || is.na(gamma) || gamma <= 0)
    stop("'gamma' must be a single positive number (Inf for the flat kernel)")
  nodes <- igraph::V(graph)$name
  A <- igraph::as_adjacency_matrix(graph, attr = "weight", sparse = FALSE)[nodes, nodes, drop = FALSE]
  adj <- A > 0
  W <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  k <- rowSums(adj); s <- rowSums(A)
  for (i in which(k > 0)) {
    nb <- which(adj[i, ])
    if (is.infinite(gamma)) { W[i, nb] <- 1 / k[i]; next }
    e <- A[i, nb]; b <- gamma * s[i] / k[i]
    z <- e^2 / (2 * b^2); v <- exp(-(z - min(z)))          # log-sum-exp: minimum exponent subtracted
    W[i, nb] <- v / sum(v)
  }
  diag(W) <- NA
  W
}
