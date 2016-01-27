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
  if( missing(x))
    stop("You need to pass a data.frame to this function...")
  
  if( !is(x,"data.frame"))
    stop("This function works on data.frames")
  
  if( !is.null(stratum)) {
    if( !(stratum %in% names(x))) {
      stop("If you specify 'stratum=' you need to give it the name of a real column.")   
    }
  }
  else {
    x$ALL <- "ALL"
    stratum <- "ALL"
  }
  
  sf <- summary.factor( x[[stratum]])
  ret <- data.frame(Stratum=names(sf), N=sf)
  df <- partition(x,stratum)
  for( locus in column_class(x,"locus")){
    ret[[locus]] <- 0
    for( strata in names(df)){
      ret[ rownames(ret)==strata, colnames(ret)==locus ] <- sum( !is.na(df[[strata]][locus]))
    }
  }
  ret
  
  return( ret )
  
}