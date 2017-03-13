#' Permutes rows and corresponding cols of a matrix.
#' 
#' This is a quick permutation of the matrix.
#' @param m A square matrix.
#' @return A permutation (of both rows and columns) of the matrix
#' @export
#' @examples 
#' m <- matrix(1:16,nrow=4)
#' m
#' permute_matrix(m)
permute_matrix <- function( m ) {
  o <- sample(1:dim(m)[1])
  p <- matrix(data = m[o, o], nrow = dim(m)[1], ncol = dim(m)[2])
  return( p )
}
