#' Asymmetric Population Graph Constructor
#'
#' Constructs a population graph that includes parameters for asymmetry
#' using entropy-based bandwidth estimation and directional edge weights.
#' @param x A numeric matrix of multivariate genetic data (e.g., from to_mv())
#' @param groups A factor indicating population membership
#' @param alpha Significance level for edge retention (default 0.05)
#' @param tol Tolerance for variance (default 1e-4)
#' @return A popgraph object with nascent b_i (node) and w_away/w_to (edge) attributes.
#' @importFrom stats uniroot
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
asymmetric_popgraph <- function(x, groups, alpha = 0.05, tol = 1.0e-4) {
  # 1. INITIAL CHECKS & DATA CLEANING
  if (missing(x)) {
    stop("Data matrix 'x' is required.")
  }
  if (is(x, "data.frame")) {
    stop("Data 'x' must be a numeric matrix. Use to_mv() first.")
  }
  if (missing(groups)) {
    stop("Grouping factor 'groups' is required.")
  }

  groups <- factor(as.character(groups))
  K <- length(levels(groups))

  # Safeguard: Minimum 2 samples per stratum
  sizes <- table(groups)
  if (any(sizes < 2)) {
    stop(paste(
      "Stratum",
      names(sizes)[sizes < 2],
      "has too few samples (n < 2)."
    ))
  }

  # Safeguard: Filter out invariant columns
  vars <- apply(x, 2, var, na.rm = TRUE)
  x <- x[, vars > tol]
  if (ncol(x) < 2) {
    stop("Insufficient multivariate variation for analysis.")
  }

  # 2. CENTROID GEOMETRY (Dyer 2004 StAMOVA logic)
  # Derived from centroid_distance.R and centroid_variance.R
  allLD <- centroid_distance(x, groups)
  allSD <- centroid_variance(x, groups)

  D <- matrix(0.0, nrow = K, ncol = K)
  for (i in 1:K) {
    for (j in i:K) {
      if (i != j) {
        p1 <- unlist(allLD[i, ])
        p2 <- unlist(allLD[j, ])
        # Euclidean distance in genetic space (R-mode)
        D[i, j] <- D[j, i] <- sqrt(sum((p1 - p2)^2))
      }
    }
  }
  rownames(D) <- colnames(D) <- rownames(allLD)

  # Original symmetric weight budget (Sum of upper triangle)
  S_original <- sum(D) / 2

  # 3. TOPOLOGY ESTIMATION (Dyer 2007 logic)
  colMeanMatrix <- matrix(colMeans(D), K, K, byrow = TRUE)
  rowMeanMatrix <- matrix(colMeans(D), K, K, byrow = FALSE)
  C <- -0.5 * (D^2 - colMeanMatrix - rowMeanMatrix + mean(D^2))

  R <- matrix(1, K, K)
  for (i in 1:K) {
    for (j in 1:K) {
      if (i != j) {
        denom <- sqrt(C[i, i] * C[j, j])
        R[i, j] <- if (!is.na(denom) && denom > tol) C[i, j] / denom else 0
      }
    }
  }

  # Partial Correlation via Generalized Inverse
  RI <- MASS::ginv(R)
  adj_matrix <- matrix(0, K, K)
  for (i in 1:(K - 1)) {
    for (j in (i + 1):K) {
      denom_pc <- sqrt(RI[i, i] * RI[j, j])
      p_cor <- if (denom_pc > tol) -RI[i, j] / denom_pc else 0
      if (abs(p_cor) > alpha) adj_matrix[i, j] <- adj_matrix[j, i] <- p_cor
    }
  }

  # 4. CONSTRUCT GRAPH
  colnames(adj_matrix) <- rownames(adj_matrix) <- rownames(D)
  graph <- igraph::graph_from_adjacency_matrix(
    adj_matrix,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
  )

  # 5. NODE PROPERTY: GENETIC GRAVITY (b_i)
  b_vector <- rep(0, K)
  names(b_vector) <- V(graph)$name

  for (i in 1:K) {
    k_i <- degree(graph)[i]

    # Handle orphans: Default to global mean distance
    if (k_i == 0) {
      b_vector[i] <- mean(D)
      next
    }

    # Target Entropy based on topological neighbors (Dyer 2007)
    target_H <- log2(k_i + 1)
    dist_row <- D[i, ]

    # Solve for b_i using entropy-matching
    tryCatch(
      {
        opt <- uniroot(
          function(b) {
            p <- exp(-(dist_row^2) / (2 * b^2))
            p[i] <- 0
            if (sum(p) == 0) {
              return(-target_H)
            }
            p <- p / sum(p)
            H <- -sum(p * log2(p + 1e-10))
            return(H - target_H)
          },
          lower = 0.001,
          upper = max(D) * 10
        )
        b_vector[i] <- opt$root
      },
      error = function(e) {
        # Fallback to local variance if root-finding fails
        b_vector[i] <- sqrt(allSD[i]) + tol
      }
    )
  }
  V(graph)$bandwidth <- b_vector

  # 6. EDGE PROPERTIES: INTROGRESSION PRESSURE (w_away / w_to)
  if (gsize(graph) > 0) {
    edge_list <- as_edgelist(graph, names = FALSE)
    w_away <- rep(0, nrow(edge_list))
    w_to <- rep(0, nrow(edge_list))

    for (e in 1:nrow(edge_list)) {
      u <- edge_list[e, 1]
      v <- edge_list[e, 2]

      # Source u's reach toward v
      denom_u <- sum(exp(-(D[u, ]^2) / (2 * V(graph)$bandwidth[u]^2)))
      w_away[e] <- if (denom_u > tol) {
        exp(-(D[u, v]^2) / (2 * V(graph)$bandwidth[u]^2)) / denom_u
      } else {
        0
      }

      # Source v's reach toward u
      denom_v <- sum(exp(-(D[v, ]^2) / (2 * V(graph)$bandwidth[v]^2)))
      w_to[e] <- if (denom_v > tol) {
        exp(-(D[v, u]^2) / (2 * V(graph)$bandwidth[v]^2)) / denom_v
      } else {
        0
      }
    }

    # Final scaling to preserve original total weight budget S
    current_S <- sum(w_away + w_to) / 2
    if (current_S > tol) {
      scaler <- S_original / current_S
      E(graph)$w_away <- w_away * scaler
      E(graph)$w_to <- w_to * scaler
    } else {
      E(graph)$w_away <- w_away
      E(graph)$w_to <- w_to
    }
  }

  class(graph) <- c("popgraph", class(graph))
  return(graph)
}
