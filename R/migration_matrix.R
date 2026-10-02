#' Create a migration matrix
#'
#' Constructs a K x K migration matrix for use in forward-time simulations.
#' Rows sum to 1; element \code{[i,j]} is the proportion of population i
#' that comes from (or goes to) population j each generation.
#'
#' @param pops Character vector of population names or an integer count.
#' @param model One of \code{"island"}, \code{"stepping_stone_1d"},
#'   \code{"stepping_stone_1d_asym"}, \code{"stepping_stone_2d"},
#'   \code{"distance"}, or \code{"custom"}.
#' @param m Base migration rate on \code{[0,1]}.
#' @param ... Additional arguments depending on model:
#'   \describe{
#'     \item{nr, nc}{Number of rows and columns for \code{"stepping_stone_2d"}.}
#'     \item{coords}{Two-column matrix of coordinates for \code{"distance"}.}
#'     \item{decay}{Decay function for \code{"distance"} (default \code{function(d) 1/d}).}
#'     \item{custom_matrix}{A K x K matrix for \code{"custom"} (rows will be normalized).}
#'     \item{m_fwd, m_rev}{For \code{"stepping_stone_1d_asym"}: per-generation rates of
#'       migration from each population to its higher-indexed neighbour (\code{m_fwd})
#'       and to its lower-indexed neighbour (\code{m_rev}); \code{m} is ignored.  Rates
#'       are not split between neighbours, so an interior population sends
#'       \code{m_fwd + m_rev} and an end population only one of them.  This is the
#'       directional stepping stone of the genetic-gravity simulations
#'       (e.g. \code{m_fwd = 0.04, m_rev = 0.01}).}
#'   }
#' @return A named K x K numeric matrix with rows summing to 1.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' migration_matrix(3, model = "island", m = 0.05)
#' migration_matrix(c("A","B","C","D"), model = "stepping_stone_1d", m = 0.1)
#' migration_matrix(5, model = "stepping_stone_1d_asym", m_fwd = 0.04, m_rev = 0.01)
migration_matrix <- function(pops, model = c("island", "stepping_stone_1d",
                                              "stepping_stone_1d_asym",
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
    stepping_stone_1d_asym = .mm_ss1d_asym(K, args),
    stepping_stone_2d = .mm_ss2d(K, m, args),
    distance = .mm_distance(K, m, args),
    custom = .mm_custom(K, args)
  )

  rownames(mat) <- colnames(mat) <- pop_names
  return(mat)
}

# ---------- internal model constructors ----------

#' @keywords internal
.mm_ss1d_asym <- function(K, args) {
  if (is.null(args$m_fwd) || is.null(args$m_rev))
    stop("model = 'stepping_stone_1d_asym' needs 'm_fwd' and 'm_rev'.")
  mf <- args$m_fwd; mr <- args$m_rev
  if (mf < 0 || mr < 0 || mf + mr > 1) stop("'m_fwd' and 'm_rev' must be >= 0 and sum to at most 1.")
  mat <- matrix(0, K, K)
  for (i in seq_len(K)) {
    if (i > 1) mat[i, i - 1] <- mr
    if (i < K) mat[i, i + 1] <- mf
    mat[i, i] <- 1 - (i > 1) * mr - (i < K) * mf
  }
  mat
}

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
