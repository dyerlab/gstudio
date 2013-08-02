#' Determine heterozygosity
#' 
#' This function will determine the heterozygosity of one or more
#'  objects of type \code{locus}.
#' @param x A \code{code} object (single or vector)
#' @return A logical flag indicating that there are at least two kinds
#'  of alleles present in the \code{locus} object.  
#' @note A haploid or NA locus is never heterozygous.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' loc <- locus( 1:2 )
#' is_heterozygote( loc )
is_heterozygote <- function( x ) { 
  if( !is(x,"locus") )
    stop("The function 'is_heterozygote' works on objects of type 'locus' ")
  
  if( length(x)>1 )
    ret <- unlist(lapply( x, is_heterozygote ))
  else {
    a <- alleles( x ) 
    if( length(a) < 2 )
      ret <- FALSE
    else 
      ret <- length(unique(a)) > 1
  }
  return( ret )
}
