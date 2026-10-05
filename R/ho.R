#' Estimate observed heterozygosity
#'  
#' Returns the general observed heterozygosity parameter
#'  from the genotypes.
#' @param x An object of type \code{locus} or a \code{data.frame} with locus columns.
#' @return The observed heterozygosity.
#' @note For stratified estimates across populations, use \code{\link{Hos}}.
#' @noRd
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' loci <- c( locus( c("A","A") ), locus( c("A","A") ), locus( c("A","B") ) )
#' Ho( loci )
Ho <- function( x ) {  
  
  if( is(x,"data.frame") ){
    locus_names <- column_class(x,class="locus")
    if( length(locus_names)==0)
      stop("Cannot estimate expected heterozygosity if there are no loci...")
    
    ret <- data.frame( Locus=locus_names, Ho=0 )
    k <- length(locus_names)
    
    for( i in 1:k) 
      ret$Ho[i] <- Ho( x[[locus_names[i]]] )
    
    if( k > 1 )
      ret <- rbind( ret, data.frame(Locus="Multilocus",Ho=mean(ret$Ho,na.rm=TRUE)))

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
 










