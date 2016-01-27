#' Estimate expected heterozygosity
#'  
#' Returns the general expected total heterozygosity parameter
#' @param x A \code{data.frame} object with \code{locus} objects 
#' @param stratum  The name of the column representing the stratum variable
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' loci <- c( locus( c("A","A") ), locus( c("A","A") ), locus( c("A","B") ) )
#' Ht( loci )
#' Ht( loci, small.sample.correction=TRUE )
Ht <- function( x, stratum ) { 
  
  if( missing(x) || missing(stratum) || !is(x,"data.frame") || !(stratum %in% names(x))) 
    stop("You need to pass both a data.frame and the name of the stratum to this function")
  
  locus_names <- column_class(x,class="locus")
  if( length(locus_names)==0)
    stop("Cannot estimate expected total heterozygosity if there are no loci...")
  
  ret <- data.frame( Locus=locus_names, Ht=0 )

  ho <- Ho( x, stratum=stratum )
  hs <- He( x, stratum=stratum )
  freqs <- frequencies( x, stratum=stratum)
  cts <- genotype_counts(x, stratum)
  K <- nrow(cts)
  
  for( locus in locus_names ) {
    nbar <-  harmonic_mean(cts[[locus]])
    xki <- freqs[ freqs$Locus==locus, ]
    xibar <- unlist(by( xki$Frequency, xki$Allele, function(x) sum(x/K )))
    ht <- 1 - sum(xibar^2) + hs$He[hs$Locus==locus]/(K*nbar) - ho$Ho[ho$Locus==locus]/(K*nbar*2)
    ret$Ht[ ret$Locus == locus ] <- ht
  }
  
  return( ret )
}
