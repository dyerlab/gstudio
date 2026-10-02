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
#' @examples
#' M <- migration_matrix(c("A", "B"), model = "island", m = 0.05)
#' ev <- migration_event(M, start = 1, end = 50)
#' ev
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
