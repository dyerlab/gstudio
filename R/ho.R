#' Estimate observed heterozygosity
#'  
#' Returns the general observed heterozygosity parameter
#'  from the frequencies
#' @param x An object of type \code{locus}
#' @param stratum A flag indicating that you want to estimate Nei's unbiased Ho
#'  across sampling locations.
#' @return The expected heterozygosity
#' @note This function can be called on a single vector of data, a \code{data.frame} of loci, 
#'  or a \code{data.frame} of \code{locus} objects across strata.  If the estimating across
#'  stratum, the unbiased estimator should be used to average across stratum and is performed
#'  by passing the appropriate stratum= argument.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' loci <- c( locus( c("A","A") ), locus( c("A","A") ), locus( c("A","B") ) )
#' Ho( loci )
Ho <- function( x, stratum=NULL ) {  
  
  if( is(x,"data.frame") ){
    locus_names <- column_class(x,class="locus")
    if( length(locus_names)==0)
      stop("Cannot estimate expected heterozygosity if there are no loci...")
    
    ret <- data.frame( Locus=locus_names, Ho=0 )
    k <- length(locus_names)
    for( i in 1:k) {
      
      # as a single sample estimate
      if( is.null(stratum)) {
        ret$Ho[i] <- Ho( x[[locus_names[i]]] )
      }
      
      # as a weighted estimate by population after Nei
      else {
        ret$Ho[i] <- mean( unlist( by( x[[locus_names[i]]], droplevels(x[[stratum]]), Ho) ) )
      }
    }
    
    if( !is.null(stratum ) ){
      ret <- rbind( ret, data.frame(Locus="Multilocus",Ho=sum(ret$Ho)/k))
    }
    

    return( ret )
  }
  
  else if( is( x, "locus")) {
    ret <- NA 
    Ninds <- sum( ploidy(x)>1 )
    Nhets <- sum( is_heterozygote(x) )
    
    if( Ninds < sum( !is.na(x) ))
      warning("Some loci were not treated as elegable to be counted for heterozygotes due to ploidy < 2.")
    
    if( Ninds > 0)
      ret <- Nhets / Ninds
    
    names(ret) <- "Ho"
    return( ret )
  }
  
  else 
    stop("How can I get expected heterozygosity from a non-locus object...")

} 
 










