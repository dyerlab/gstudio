#' Directional Asymmetry Indices for a Weighted Population Graph
#'
#' @description
#' Computes directional introgression pressures and an asymmetry index for
#' every edge in an undirected, weighted population graph. For each node
#' \eqn{i}, a local bandwidth \eqn{b_i} is estimated as the mean edge weight
#' over its immediate neighborhood (topological degree divided into strength).
#' A Gaussian kernel is then evaluated over that neighborhood to produce the
#' conditional introgression pressure \eqn{w_{i \to j}}, the probability that
#' genetic signal originating at node \eqn{i} reaches neighbor \eqn{j} rather
#' than any other connected population.
#'
#' @details
#' For each node \eqn{i} let \eqn{\mathcal{N}(i)} denote its set of graph
#' neighbors and let \eqn{w_{ij}} be the edge weight (genetic distance) between
#' \eqn{i} and \eqn{j \in \mathcal{N}(i)}.  The local bandwidth is
#'
#' \deqn{b_i = \frac{\sum_{j \in \mathcal{N}(i)} w_{ij}}{|\mathcal{N}(i)|}}
#'
#' The directional weight from \eqn{i} toward \eqn{j} is the Gaussian kernel
#' evaluated at \eqn{w_{ij}} and normalised over all neighbors of \eqn{i}:
#'
#' \deqn{w_{i \to j} = \frac{\exp\!\left(-w_{ij}^2 \,/\, 2b_i^2\right)}
#'       {\displaystyle\sum_{k \in \mathcal{N}(i)}
#'        \exp\!\left(-w_{ik}^2 \,/\, 2b_i^2\right)}}
#'
#' The asymmetry index for the edge \eqn{(i, j)} is
#'
#' \deqn{\Delta_{ij} = w_{i \to j} - w_{j \to i}}
#'
#' Positive values indicate net introgression pressure from \eqn{i} toward
#' \eqn{j}; negative values indicate net pressure in the reverse direction.
#'
#' Bandwidth estimation is deliberately local: because a population graph is a
#' Markov Random Field, conditional independence holds across non-adjacent nodes
#' given the separating set, so the neighborhood edge weights are sufficient
#' statistics for the decay of genetic signal at each node.
#'
#' The graph topology and original \code{weight} edge attribute are not
#' modified.  Three new edge attributes and one new vertex attribute are
#' appended to the returned object.
#'
#' @param graph An undirected \code{popgraph} or \code{igraph} object with a
#'   numeric \code{weight} edge attribute representing pairwise genetic
#'   distances between populations.  Isolated vertices (degree zero) are
#'   permitted but contribute no edge attributes.
#'
#' @return The input graph, unchanged in topology and edge weights, with the
#'   following additions:
#'   \describe{
#'     \item{\code{V(graph)$bandwidth}}{Numeric vertex attribute. The local
#'       bandwidth \eqn{b_i = \bar{w}_i} (mean edge weight) used in the
#'       Gaussian kernel for node \eqn{i}.}
#'     \item{\code{E(graph)$w_away}}{Numeric edge attribute. The conditional
#'       probability \eqn{w_{i \to j}} that genetic signal from the first
#'       endpoint reaches the second, as ordered by
#'       \code{\link[igraph]{as_edgelist}}.}
#'     \item{\code{E(graph)$w_to}}{Numeric edge attribute. The conditional
#'       probability \eqn{w_{j \to i}} in the reverse direction.}
#'     \item{\code{E(graph)$delta}}{Numeric edge attribute. The asymmetry
#'       index \eqn{\Delta_{ij} = w_{away} - w_{to}}.  Values range from
#'       \eqn{-1} to \eqn{1}; zero indicates symmetric connectivity.}
#'   }
#'
#' @seealso \code{\link{popgraph}} to construct the population graph that
#'   serves as input; \code{\link{asymmetric_weights}} for an alternative
#'   formulation using perplexity-based bandwidth search.
#'
#' @references
#' Dyer RJ, Nason JD (2004) Population Graphs: the graph theoretic shape of
#' genetic structure. \emph{Evolution} \strong{58}: 1605--1615.
#'
#' Dyer RJ (2015) Population Graphs and Landscape Genetics.
#' \emph{Annual Review of Ecology, Evolution, and Systematics}
#' \strong{46}: 327--342.
#'
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#'
#' @examples
#' library(igraph)
#'
#' # Build a small weighted graph directly from an adjacency matrix
#' A <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
#' A["A", "B"] <- A["B", "A"] <- 2.1
#' A["A", "C"] <- A["C", "A"] <- 3.4
#' A["B", "C"] <- A["C", "B"] <- 1.2
#' A["C", "D"] <- A["D", "C"] <- 2.8
#'
#' g <- graph_from_adjacency_matrix(A, mode = "undirected",
#'                                  weighted = TRUE, diag = FALSE)
#' class(g) <- c("popgraph", class(g))
#'
#' g2 <- graph_asymmetries(g)
#'
#' # Inspect directional edge weights
#' data.frame(
#'   from  = as_edgelist(g2)[, 1],
#'   to    = as_edgelist(g2)[, 2],
#'   w_away = round(E(g2)$w_away, 3),
#'   w_to   = round(E(g2)$w_to,   3),
#'   delta  = round(E(g2)$delta,  3)
#' )
#'
#' @importFrom igraph is_igraph is_directed E V strength degree neighbors
#'   get_edge_ids as_edgelist
#' @export
graph_asymmetries <- function(graph) {
  if (!igraph::is_igraph(graph))
    stop("'graph' must be an igraph object")
  if (igraph::is_directed(graph))
    stop("'graph' must be undirected")
  if (is.null(igraph::E(graph)$weight))
    stop("'graph' must have a numeric 'weight' edge attribute")

  nodes <- igraph::V(graph)$name

  # Node bandwidth: mean edge weight over local neighborhood (b_i = s_i / k_i)
  b <- igraph::strength(graph) / igraph::degree(graph)
  names(b) <- nodes
  igraph::V(graph)$bandwidth <- b

  # Precompute the normalised Gaussian kernel for every node over its
  # neighborhood. Stored as a named list for O(1) edge-level lookup.
  node_kernels <- lapply(nodes, function(u) {
    nbs <- igraph::neighbors(graph, u, mode = "all")$name
    d <- setNames(
      sapply(nbs, function(n) {
        igraph::E(graph)[igraph::get_edge_ids(graph, c(u, n),
                                              directed = FALSE)]$weight
      }),
      nbs
    )
    k <- exp(-d^2 / (2 * b[u]^2))
    k / sum(k)
  })
  names(node_kernels) <- nodes

  # Assign w_away, w_to, and delta per edge.
  # Edge order from as_edgelist() matches E(graph) iteration order.
  edges  <- igraph::as_edgelist(graph, names = TRUE)
  ne     <- nrow(edges)
  w_away <- numeric(ne)
  w_to   <- numeric(ne)

  for (e in seq_len(ne)) {
    u <- edges[e, 1]
    v <- edges[e, 2]
    w_away[e] <- node_kernels[[u]][v]
    w_to[e]   <- node_kernels[[v]][u]
  }

  igraph::E(graph)$w_away <- w_away
  igraph::E(graph)$w_to   <- w_to
  igraph::E(graph)$delta  <- w_away - w_to

  return(graph)
}
