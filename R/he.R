#' Estimate expected heterozygosity
#'  
#' Returns the general expected heterozygosity parameter
#'  from the frequencies
#' @param x Either a \code{data.frame} object with \code{locus} objects or 
#'  a vector or \code{locus} objects.
#' @param small.N Apply the 2N/(2N-1) correction to the data
#'  for small sample sizes.
#' @param stratum  This optional argument makes He estimate Nei's Hs parameter taking
#'  into consideration population subdivision.  This is an unbiased estimator.
#' @return The expected heterozygosity as a numeric or a \code{data.frame} if 
#'  several loci are passed.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' loci <- c( locus( c("A","A") ), locus( c("A","A") ), locus( c("A","B") ) )
#' He( loci )
#' He( loci, small.N=TRUE )
He <- function( x, small.N=FALSE, stratum=NULL ) { 

  if( is(x,"data.frame") ){
    x <- droplevels(x)
    if( !is.null(stratum) && !(stratum %in% names(x) ) )
      stop("If you are going to specify a column for stratum, make sure it is actually in the data.frame you pass...")
    
    locus_names <- column_class(x,class="locus")
    if( length(locus_names)==0)
      stop("Cannot estimate expected heterozygosity if there are no loci...")
    
    ret <- data.frame( Locus=locus_names, He=0 )
    k <- length(locus_names)
    if( is.null( stratum ) ) {
      for( i in 1:k) {
        if( is.null(stratum) ){
          ret$He[i] <- He( x[[locus_names[i]]], small.N, ... )  
        }
      }
    } else {
      ho <- Ho( x, stratum=stratum )
      cts <- genotype_counts(x, stratum)
      K <- nrow(cts)
      freqs <- frequencies( x, stratum=stratum)
      
      for( locus in locus_names ) {
        nbar <-  harmonic_mean(cts[[locus]])
        xki <- freqs[ freqs$Locus==locus, ]
        x2ibar <- unlist(by( xki$Frequency, xki$Allele, function(x) sum(x^2/K) ))
        hs <- nbar/(nbar-1) * ( 1 - sum(x2ibar) - ho$Ho[ ho$Locus==locus]/(2*nbar) )
        ret$He[ ret$Locus == locus ] <- hs
      }
      
    }
    
    return( ret )
  }
  
  else if( is( x, "locus")){
    
    df <- frequencies(x)
    he <- 1.0 - sum( df$Frequency**2 )
    Ninds <- sum( ploidy(x)>1 )
    if( small.N )
      he <- he * 2 * (Ninds)/( 2 * Ninds - 1 )
    
    return( he )    
  }
  
  else 
    stop("he() is only available for objects of type 'locus' or type 'data.frame'")
  
}
