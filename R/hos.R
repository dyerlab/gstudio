#' Estimate observed heterozygosity corrected for sample size after Nei
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
#' data(arapat)
#' Hos(arapat)
#' loci <- c( locus( c("A","A") ), locus( c("A","A") ), locus( c("A","B") ) )
#' Hos( loci )
Hos <- function( x, stratum="Population" ) {
  
  if( is(x,"data.frame") ){
    if( !(stratum %in% names(x)))
      stop("You need to specify a stratum in the data.frame for the Hos() function.")
    locus_names <- column_class(x,class="locus")
    if( length(locus_names)==0)
      stop("Cannot estimate expected heterozygosity if there are no loci...")
    
    ret <- data.frame( Locus=locus_names, Hos=0 )
    k <- length(locus_names)
    for( i in 1:k) {
      
      # as a single sample estimate
      if( is.null(stratum)) {
        ret$Hos[i] <- Hos( x[[locus_names[i]]] )
      }
      
      # as a weighted estimate by population after Nei
      else {
        if( is( x[[stratum]], "factor"))
          x[[stratum]] <- droplevels(x[[stratum]])
        ret$Hos[i] <- mean( unlist( by( x[[locus_names[i]]], x[[stratum]], Hos) ), na.rm=TRUE )
      }
    }
    
    if( length( locus_names ) > 1 ){
      hos <- ret$Hos[ !is.na(ret$Hos)]
      k <- length(hos)
      ret <- rbind( ret, data.frame(Locus="Multilocus",Hos=sum(hos)/k))
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
    
    names(ret) <- "Hos"
    return( ret )
  }
  
  else 
    stop("How can I get expected heterozygosity from a non-locus object...")

} 
 










