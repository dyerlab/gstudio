#' Returns quickly indices for a matrix of size N
#' 
#' This is used in conjunction with apply to get around
#'  doing nested loops in R.  This function is slightly
#'  faster than expand.grid. This function was lifted
#'  from a discussion on stackoverflow.
#' @param seq The first Sequence.
#' @param only.unique Makes sure to return only i,j and not 
#'  both [i,j] and [j,i] values
#' @return A matrix of indicies
#' @export
indices <- function( seq, only.unique=TRUE ) {
  ret <- cbind( c(t(matrix(rep.int(seq, length(seq)), nrow=length(seq)))), rep.int(seq, length(seq)) )
  if( only.unique)
    ret <- ret[ ret[,1] < ret[,2],  ]
  return(ret)
} 



