#' Estimate expected heterozygosity
#'  
#' Returns the general expected heterozygosity parameter
#'  from the frequencies
#' @param x Either a \code{data.frame} object with \code{locus} objects or 
#'  a vector or \code{locus} objects.
#' @param stratum  This optional argument makes He estimate Nei's Hs parameter taking
#'  into consideration population subdivision.  This is an unbiased estimator.
#' @param small.N Apply the 2N/(2N-1) correction to the data
#'  for small sample sizes.
#' @return The expected heterozygosity as a numeric or a \code{data.frame} if 
#'  several loci are passed.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' data(arapat)
#' Hes( arapat )
Hes <- function( x, stratum="Population", small.N=FALSE ) { 

  if( is(x,"data.frame") ){
    x <- droplevels(x)
    
    if( !( stratum %in% names(x) ) )
      stop("You must specify a column for stratum and make sure it is actually in the data.frame you pass...")
    
    locus_names <- column_class(x,class="locus")
    if( length(locus_names)==0)
      stop("Cannot estimate expected heterozygosity if there are no loci...")
    
    ret <- data.frame( Locus=locus_names, Hes=0 )
    k <- length(locus_names)
    if( is.null( stratum ) ) {
      for( i in 1:k) {
        ret$Hes[i] <- Hes( x[[locus_names[i]]], small.N=small.N )  
      }
    } else {
      hos <- Hos( x, stratum=stratum )
      cts <- genotype_counts(x, stratum)
      K <- nrow(cts)
      if( !is.null(stratum)) {
        freqs <- frequencies( x, stratum=stratum)
      }
      else
        freqs <- frequencies( x )
      
      for( locus in locus_names ) {
        
        if( any( cts[[locus]] == 0 )) {
          ret$Hes[ ret$Locus == locus ] <- NA
        }
        
        else {
          nbar <-  harmonic_mean(cts[[locus]])
          xki <- freqs[ freqs$Locus==locus, ]
          x2ibar <- unlist(by( xki$Frequency, xki$Allele, function(x) sum(x^2/K) ))
          hs <- nbar/(nbar-1) * ( 1 - sum(x2ibar) - hos$Hos[ hos$Locus==locus]/(2*nbar) )
          if( !is.null(stratum)){
            ret$Hes[ ret$Locus == locus ] <- hs 
            ret$He <- NULL
          }
          else {
            ret$Hes[ ret$Locus == locus ] <- hs
          }
        }
      }
      
      if( length(locus_names) > 1 ){
        if( !is.null( locus_names ) ){
          hes <- ret$Hes[ !is.na(ret$Hes)]
          k <- length(hes)
          ret <- rbind( ret, data.frame(Locus="Multilocus",Hes=sum(hes)/k))
        }
      }
        
      
    }
    
    return( ret )
  }
  
  else if( is( x, "locus")){
    
    df <- frequencies(x)
    hes <- 1.0 - sum( df$Frequency**2 )
    Ninds <- sum( ploidy(x)>1 )
    if( small.N )
      hes <- hes * 2 * (Ninds)/( 2 * Ninds - 1 )
    
    return( hes )    
  }
  
  else 
    stop("hes() is only available for objects of type 'locus' or type 'data.frame'")
  
}
