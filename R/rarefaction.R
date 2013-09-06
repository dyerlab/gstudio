#' Does allelic diversity rarefaction
#' 
#' This funciton subsamples the data with a smaller sample size and estimates the 
#'  density of the diversity parameter.
#' @param x The raw data as a \code{locus} vector.
#' @param mode The mode passed to \code{allelic_diversity}.
#' @param size The smaller sample size to use.
#' @param nperm The number of times to subsample the data (default 999)
#' @return A vector of permuted values.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
rarefaction <- function( x, mode="Ae", size=0, nperm=999 ) {
  if( !is(x,"locus"))
    stop("This funciton only works with vectors of locus objects")
  if( size==0 )
    stop("You need to specify the size of population to use for rarefaction.")
  if( size > length(x) )
    warning("You are asking to permute populations larger than the observed... ")
  
  ret <- rep(NA, nperm)
  for( i in 1:nperm )
    ret[i] <- genetic_diversity( sample(x,size=size,replace=TRUE), mode=mode )
  ret <- unlist(ret)
  
  return( ret )
}
