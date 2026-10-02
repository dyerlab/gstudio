#' @keywords internal
#' @noRd
.validate_asymmetry_data <- function(data, groups) {
  if (is.null(data) || is.null(groups))
    stop("both 'data' and 'groups' are required.")
  if (!is.matrix(data))
    stop("'data' must be a numeric matrix (use to_mv() on your genotypes).")
  if (length(groups) != nrow(data))
    stop("'groups' must have one entry per row of 'data'.")
  invisible(TRUE)
}
