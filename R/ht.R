#' Estimate expected heterozygosity
#'  
#' Returns the general expected total heterozygosity parameter
#' @param x A \code{data.frame} object with \code{locus} objects 
#' @param stratum  The name of the column representing the stratum variable (default=Population)
#' @importFrom dplyr select all_of
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' loci1 <- c( locus( c("A","A") ), locus( c("A","A") ), locus( c("A","B")))
#' loci2 <- c( locus( c("A","A") ), locus( c("A","B")), locus(c("B","B")))
#' df <- data.frame( Population=c("One","One","One","Two","Two","Two"), Locus=c(loci1,loci2) )
#' Ht( df )
Ht <- function( x, stratum="Population" ) { 
  
  if( missing(x) || !(stratum %in% names(x))) 
    stop("You need to pass both a data.frame and the name of the stratum to this function")
  
  locus_names <- column_class(x,class="locus")
  if( length(locus_names)==0)
    stop("Cannot estimate expected total heterozygosity if there are no loci...")
  
  ret <- data.frame( Locus=locus_names, Ht=0 )
  
  
  

  for( locus in locus_names ) {

    # Catch strata of small size and delete.
    x |> 
      dplyr::select( dplyr::all_of(stratum), dplyr::all_of(locus) ) -> tmp 
    tmp <- tmp[ !is.na(tmp[,2]),]
    
    numSamples <- nrow(tmp)
    if( nrow(x) != numSamples) { 
      warning(paste(locus,"had missing data, dropped individuals.")) 
    }
    
    t <- table( tmp[,1])
    keep <- names(t[ t>2 ])
    tmp <- droplevels( tmp[ (tmp[,1] %in% keep), ] )
    
    # issue warning
    if( nrow( tmp ) != numSamples ) { 
      warning(paste(locus, "dropped strata (must have at least 3 individuals)") )
    }
    
    ho <- Hos( tmp, stratum=stratum )
    hs <- Hes( tmp, stratum=stratum )
    freqs <- frequencies( tmp, stratum=stratum)
    cts <- genotype_counts(tmp, stratum)
    K <- nrow(cts)
    nbar <-  harmonic_mean(cts[[locus]])
    xki <- freqs[ freqs$Locus==locus, ]
    xibar <- unlist(by( xki$Frequency, xki$Allele, function(x) sum(x/K )))
    ht <- 1 - sum(xibar^2) + hs$Hes[hs$Locus==locus]/(K*nbar) - ho$Hos[ho$Locus==locus]/(K*nbar*2)
    ret$Ht[ ret$Locus == locus ] <- ht
  }
  
  return( ret )
}
