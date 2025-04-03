#' This is a quick and dirty harmonic mean calculation
#' 
#' This estimates the harmonic mean.
#' @param x a vector of values
#' @return The harmonic mean
#' @export
#' @examples 
#'  x <- c(1,2,3,4,5)
#'  harmonic_mean(x)
#'  x <- c(1,2,3,4,5,0)
#'  harmonic_mean(x)
harmonic_mean <- function(x) {
  if( any(x==0) ) {
    warning("Asking for harmonic mean with zero entry, I'm gonna drop it and give you the rest.")
    x <- x[ x != 0]
    return( 1/mean(1/x) )
  } else { 
    return( 1/mean(1/x) )
  }
  
} 