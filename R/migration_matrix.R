#' Create a migration matrix
#'
#' Constructs a K x K migration matrix for use in forward-time simulations.
#' Rows sum to 1; element \code{[i,j]} is the proportion of population i
#' that comes from (or goes to) population j each generation.
#'
#' @param pops Character vector of population names or an integer count.
#' @param model One of \code{"island"}, \code{"stepping_stone_1d"},
#'   \code{"stepping_stone_2d"}, \code{"distance"}, or \code{"custom"}.
#' @param m Base migration rate on \code{[0,1]}.
#' @param ... Additional arguments depending on model:
#'   \describe{
#'     \item{nr, nc}{Number of rows and columns for \code{"stepping_stone_2d"}.}
#'     \item{coords}{Two-column matrix of coordinates for \code{"distance"}.}
#'     \item{decay}{Decay function for \code{"distance"} (default \code{function(d) 1/d}).}
#'     \item{custom_matrix}{A K x K matrix for \code{"custom"} (rows will be normalized).}
#'   }
#' @return A named K x K numeric matrix with rows summing to 1.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' migration_matrix(3, model = "island", m = 0.05)
#' migration_matrix(c("A","B","C","D"), model = "stepping_stone_1d", m = 0.1)
migration_matrix <- function(pops, model = c("island", "stepping_stone_1d",
                                              "stepping_stone_2d", "distance",
                                              "custom"),
                             m = 0.1, ...) {
  model <- match.arg(model)

  if (is.numeric(pops) && length(pops) == 1) {
    K <- as.integer(pops)
    pop_names <- paste0("Pop", seq_len(K))
  } else {
    pop_names <- as.character(pops)
    K <- length(pop_names)
  }

  if (K < 2)
    stop("Need at least 2 populations.")
  if (m < 0 || m > 1)
    stop("Migration rate m must be on [0,1].")

  args <- list(...)

  mat <- switch(model,
    island = .mm_island(K, m),
    stepping_stone_1d = .mm_ss1d(K, m),
    stepping_stone_2d = .mm_ss2d(K, m, args),
    distance = .mm_distance(K, m, args),
    custom = .mm_custom(K, args)
  )

  rownames(mat) <- colnames(mat) <- pop_names
  return(mat)
}


#' Create a migration event for temporal regime changes
#'
#' A migration event pairs a migration matrix with a generation interval
#' during which it is active.
#'
#' @param matrix A migration matrix (as produced by \code{migration_matrix}).
#' @param start Generation at which this event starts (>= 1).
#' @param end Generation at which this event ends (>= start, or \code{NULL}
#'   for indefinite).
#' @return A list of class \code{"migration_event"} with elements
#'   \code{matrix}, \code{start}, and \code{end}.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
migration_event <- function(matrix, start = 1, end = NULL) {
  if (!is.matrix(matrix))
    stop("matrix must be a matrix.")
  if (nrow(matrix) != ncol(matrix))
    stop("Migration matrix must be square.")
  if (any(abs(rowSums(matrix) - 1) > 1e-8))
    stop("Rows of migration matrix must sum to 1.")
  if (start < 1)
    stop("start must be >= 1.")
  if (!is.null(end) && end < start)
    stop("end must be >= start.")
  ret <- list(matrix = matrix, start = start, end = end)
  class(ret) <- "migration_event"
  return(ret)
}


# ---------- internal model constructors ----------

#' @keywords internal
.mm_island <- function(K, m) {
  off_diag <- m / (K - 1)
  mat <- matrix(off_diag, nrow = K, ncol = K)
  diag(mat) <- 1 - m
  return(mat)
}

#' @keywords internal
.mm_ss1d <- function(K, m) {
  mat <- matrix(0, nrow = K, ncol = K)
  for (i in seq_len(K)) {
    neighbors <- integer(0)
    if (i > 1) neighbors <- c(neighbors, i - 1)
    if (i < K) neighbors <- c(neighbors, i + 1)
    n_nbr <- length(neighbors)
    rate_per_nbr <- m / n_nbr
    for (j in neighbors)
      mat[i, j] <- rate_per_nbr
    mat[i, i] <- 1 - m
  }
  return(mat)
}

#' @keywords internal
.mm_ss2d <- function(K, m, args) {
  nr <- args$nr
  nc <- args$nc
  if (is.null(nr) || is.null(nc))
    stop("stepping_stone_2d requires nr and nc arguments.")
  if (nr * nc != K)
    stop("nr * nc must equal the number of populations.")

  mat <- matrix(0, nrow = K, ncol = K)
  for (idx in seq_len(K)) {
    r <- ((idx - 1) %/% nc) + 1
    c_pos <- ((idx - 1) %% nc) + 1
    neighbors <- integer(0)
    if (r > 1)  neighbors <- c(neighbors, (r - 2) * nc + c_pos)
    if (r < nr) neighbors <- c(neighbors, r * nc + c_pos)
    if (c_pos > 1)  neighbors <- c(neighbors, (r - 1) * nc + (c_pos - 1))
    if (c_pos < nc) neighbors <- c(neighbors, (r - 1) * nc + (c_pos + 1))
    n_nbr <- length(neighbors)
    rate_per_nbr <- m / n_nbr
    for (j in neighbors)
      mat[idx, j] <- rate_per_nbr
    mat[idx, idx] <- 1 - m
  }
  return(mat)
}

#' @keywords internal
.mm_distance <- function(K, m, args) {
  coords <- args$coords
  decay <- args$decay
  if (is.null(coords))
    stop("distance model requires coords argument.")
  if (is.null(decay))
    decay <- function(d) 1 / d
  if (nrow(coords) != K)
    stop("coords must have the same number of rows as populations.")

  dmat <- as.matrix(stats::dist(coords))
  mat <- matrix(0, nrow = K, ncol = K)
  for (i in seq_len(K)) {
    weights <- decay(dmat[i, -i])
    weights <- weights / sum(weights) * m
    mat[i, -i] <- weights
    mat[i, i] <- 1 - m
  }
  return(mat)
}

#' @keywords internal
.mm_custom <- function(K, args) {
  custom_matrix <- args$custom_matrix
  if (is.null(custom_matrix))
    stop("custom model requires custom_matrix argument.")
  if (nrow(custom_matrix) != K || ncol(custom_matrix) != K)
    stop("custom_matrix dimensions must match the number of populations.")
  # Normalize rows to sum to 1
  mat <- custom_matrix / rowSums(custom_matrix)
  return(mat)
}
