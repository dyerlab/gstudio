#' Asymmetric Edge Weights
#'  This uses a method inspired by t-distributed Stochastic Neighbor Embedding
#'  to estimate relative weights of edges in a population graph using local
#'  embedding and Markov properties.
#' @param graph An existing \code{popgraph} object (undirected).
#' @param perplexity Target perplexity for the per-node bandwidth search (default 4).
#' @param tol Convergence tolerance for the binary search on sigma (default 1e-5).
#' @param max_iter Maximum iterations for the sigma binary search (default 100).
#' @return A data.frame with one row per undirected edge and columns:
#'   \describe{
#'     \item{i}{Name of the first node.}
#'     \item{j}{Name of the second node.}
#'     \item{pij}{Conditional probability of j given i (\eqn{p_{j|i}}), normalised across i's neighbours.}
#'     \item{pji}{Conditional probability of i given j (\eqn{p_{i|j}}), normalised across j's neighbours.}
#'     \item{cGD}{Conditional genetic distance (edge weight) from the input graph.}
#'     \item{dij}{Share of cGD allocated in the i→j direction: \code{cGD * pij / (pij + pji)}.}
#'     \item{dji}{Share of cGD allocated in the j→i direction: \code{cGD * pji / (pij + pji)}.}
#'     \item{Delta}{Directional bias: \code{dij - dji} (positive favours i→j).}
#'   }
#'   By construction \code{dij + dji == cGD} for every edge, so the total graph
#'   distance is preserved.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
asymmetric_weights <- function(graph, perplexity = 4, tol = 1e-5, max_iter = 100) {
  if (!igraph::is_igraph(graph)) stop("Input must be an igraph object")
  if (igraph::is_directed(graph)) stop("Input graph must be undirected")

  nodes <- igraph::V(graph)$name

  find_sigma <- function(distances, target_perplexity) {
    if (length(distances) == 1) return(1e-3)
    sigma_min <- 1e-6
    sigma_max <- 10
    for (iter in 1:max_iter) {
      sigma <- (sigma_min + sigma_max) / 2
      p <- exp(-distances^2 / (2 * sigma^2))
      p <- p / sum(p)
      perp <- 2^shannon_entropy(p)
      if (abs(perp - target_perplexity) < tol) break
      if (perp > target_perplexity) sigma_max <- sigma else sigma_min <- sigma
    }
    return(sigma)
  }

  # Collect all directed conditional probabilities into a lookup matrix
  prob <- matrix(0, nrow = length(nodes), ncol = length(nodes),
                 dimnames = list(nodes, nodes))

  for (i in seq_along(nodes)) {
    ni <- nodes[i]
    neighbors_i <- igraph::neighbors(graph, ni, mode = "all")$name
    if (length(neighbors_i) == 0) next

    dists <- sapply(neighbors_i, function(nj) {
      igraph::E(graph)[igraph::get_edge_ids(graph, c(ni, nj), directed = FALSE)]$weight
    })

    if (length(dists) == 1L) {
      p <- 1
    } else {
      sigma_i <- find_sigma(dists, perplexity)
      p <- exp(-dists^2 / (2 * sigma_i^2))
      p <- p / sum(p)
    }
    prob[ni, neighbors_i] <- p
  }

  # Build one row per undirected edge
  edges <- igraph::as_edgelist(graph, names = TRUE)
  rows <- vector("list", nrow(edges))
  for (k in seq_len(nrow(edges))) {
    n1  <- edges[k, 1]
    n2  <- edges[k, 2]
    e_id <- igraph::get_edge_ids(graph, c(n1, n2), directed = FALSE)
    cGD <- igraph::E(graph)[e_id]$weight
    pij <- prob[n1, n2]
    pji <- prob[n2, n1]
    s   <- pij + pji
    dij <- cGD * pij / s
    dji <- cGD * pji / s

    rows[[k]] <- data.frame(i = n1, j = n2, 
                            pij, pji,
                            cGD,
                            dij, dji,
                            Delta = dij - dji )
  }

  return(do.call(rbind, rows))
}