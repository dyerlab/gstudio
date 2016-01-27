#' Returns the number of non-missing genotypes per stratum
#' 
#' This function provides a quick summary of the observed data, by stratum (optional)
#'  of the number of samples without missing data.
#' @param x A \code{data.frame}
#' @param stratum The name of the column to use as a stratum.  If not given, the entire
#'  data set is used (default=NULL which will lump all data).
#' @return A \code{data.frame} with Stratum (set to "All" if not passed as param) and the
#'  number of observations without missing genetic data per column.
#' @author Rodney J. Dyer \email{rjdyer@vcu.edu}
#' 

genotype_counts <- function( x, stratum=NULL ) {
  if( !is(x,"data.frame"))
    stop("This function works on data.frames")
  
  if( !is.null(stratum) & !(stratum %in% names(x)))
    stop("If you specify 'stratum=' you need to give it the name of a real column.")
  
  

  if( is.null(stratum) ) {
    x$ALL <- "All"
    stratum <- "All"
  }
    
  sf <- summary.factor( x[[stratum]])
  
  
  ret <- data.frame(Stratum=names(sf), N=sf)
  for( locus in column_class( x, "locus" ) ){
    missing <- which(!is.na(x[[locus]]))
    t <- table( x[[stratum]][missing] )
    
    ret[[locus]] <- tapply( x[[locus]][missing], x[[stratum]]), length )
  }
  
  
  ret
  return( ret )
  
}