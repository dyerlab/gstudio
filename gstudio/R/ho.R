#' Estimate observed heterozygosity
#'  
#' Returns the general observed heterozygosity parameter
#'  from the frequencies
#' @param x An object of type \code{locus}
#' @return The expected heterozygosity
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' 
#' loci <- c( locus( c("A","A") ), locus( c("A","A") ), locus( c("A","B") ) )
#' He( loci )
#' He( loci, small.sample.correction=TRUE )
#' 
Ho <- function( x ) {  
  if( !is(x,"locus") )
    stop("How can I get expected heterozygosity from a non-locus object...")
  
  ret <- NA 
  Ninds <- sum( ploidy(x)>1 )
  Nhets <- sum( is.heterozygote(x) )
  
  if( Ninds < sum( !is.na(x) ))
    warning("Some loci were not treated as elegable to be counted for heterozygotes due to ploidy < 2.")
  
  if( Ninds > 0)
    ret <- Nhets / Ninds
  
  names(ret) <- "Ho"
  return( ret )
} 
 










