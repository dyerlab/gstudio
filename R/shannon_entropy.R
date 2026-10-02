#' Shannon Entry Function
#'  The degree of uncertainty in a system based on probability and information theory.
#' @param p The probility
#' @return An estimate of entropy
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
#' @examples
#' p <- c(0.5, 0.25, 0.25)
#' shannon_entropy(p)
shannon_entropy <- function(p) {
  p <- p[p > 0]
  -sum(p * log2(p))
}
