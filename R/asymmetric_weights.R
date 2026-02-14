#' Asymmetric Edge Weights
#'  This uses a method inspired by t-distributed Stochastic Neighbor Embedding
#'  to estimate relative weights of edges in a population graph using local 
#'  embedding and Markov properties.
#' @param graph An existing ``popgraph`` object.
#' @param perplexity A target perplexity value for the bandwidth search (default 4).
#' @param tol A minimal tolerance for variance values
#' @param max_iter An upper limit on the iterations necessary for finding a solution.
#' @return A directional population graph.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
asymmetric_weights <- function(graph, perplexity = 4, tol = 1e-5, max_iter = 100) {
  if (!igraph::is_igraph(graph)) stop("Input must be an igraph object")
  if (igraph::is_directed(graph)) stop("Input graph must be undirected")
  
  nodes <- igraph::V(graph)$name
  N <- length(nodes)
  edge_list <- list()
  edge_weights <- c()
  
  # Binary search for sigma_i given distances and target perplexity
  find_sigma <- function(distances, target_perplexity) {
    if (length(distances) == 1) return(1e-3)  # fallback for singleton
    sigma_min <- 1e-6
    sigma_max <- 10
    for (iter in 1:max_iter) {
      sigma <- (sigma_min + sigma_max) / 2
      p_ji <- exp(-distances^2 / (2 * sigma^2))
      p_ji <- p_ji / sum(p_ji)
      perp <- 2^shannon_entropy(p_ji)
      
      if (abs(perp - target_perplexity) < tol) break
      if (perp > target_perplexity) sigma_max <- sigma else sigma_min <- sigma
    }
    return(sigma)
  }
  
  for (i in seq_along(nodes)) {
    ni <- nodes[i]
    neighbors_i <- igraph::neighbors(graph, ni, mode = "all")$name
    if (length(neighbors_i) == 0) next
    
    # Get distances to neighbors
    dists <- sapply(neighbors_i, function(nj) {
      e_id <- igraph::get_edge_ids(graph, c(ni, nj), directed = FALSE)
      igraph::E(graph)[e_id]$weight
    })
    
    sigma_i <- find_sigma(dists, perplexity)
    
    # Compute conditional probabilities
    p_ji <- exp(-dists^2 / (2 * sigma_i^2))
    p_ji <- p_ji / sum(p_ji)
    
    # Add directed edges i -> j with weight p_{j|i}
    for (j in seq_along(neighbors_i)) {
      edge_list <- append(edge_list, list(c(ni, neighbors_i[j])))
      edge_weights <- c(edge_weights, p_ji[j])
    }
  }
  
  # Create directed graph
  edge_matrix <- do.call(rbind, edge_list)
  g_dir <- igraph::graph_from_edgelist(edge_matrix, directed = TRUE)
  igraph::E(g_dir)$weight <- edge_weights
  igraph::V(g_dir)$name <- nodes
  
  return( as.popgraph( g_dir ) )
}