#' Shannon Entry Function
#'  The degree of uncertainty in a system based on probability and information theory.
#' @param p The probility
#' @return An estimate of entropy
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
shannon_entropy <- function(p) {
  p <- p[p > 0]
  -sum(p * log2(p))
}
