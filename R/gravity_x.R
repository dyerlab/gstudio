#' @keywords internal
.gravity_x <- function(x, nodes) {
  if (is.null(x)) return(NULL)
  if (!is.null(names(x))) {
    if (!all(nodes %in% names(x))) stop("'x' must be named for every node in the graph")
    return(as.numeric(x[nodes]))
  }
  if (length(x) != length(nodes)) stop("'x' must have one value per node (or be named by node)")
  as.numeric(x)
}
