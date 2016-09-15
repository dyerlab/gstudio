#' Returns the max digits of all loci passed.
#' 
#' This is a convienence function that returns the number of digits for a locus
#'  or set of loci.
#' @param x A \code{data.frame} or vector of \code{locus} objects
#' @return An integer depicting the maximum number of digits in alleles at the 
#'  locus.
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples 
#' loci <- c( locus( c(1,12) ), locus( c(2,22) ), locus( c(2222,2) ))
#' maximum_allele_size( loci )

maximum_allele_size <- function( x ) {
  
  if( missing(x) )
    stop("You need to pass SOME KIND OF DATA to this function.")
  
  if( is(x,"data.frame")){
    loci <- column_class(x, "locus")
    ret <- NULL
    for( locus in loci ) {
      mx <- maximum_allele_size( x[[locus]] )
      if( is.null(ret) )
        ret <- mx
      else if( mx > ret )
        ret <- mx
    }
    return( ret )
  }
  
  else if( is(x, "locus" ) ){
    return( max(nchar(as.character(alleles(x)))) )
  }
  
  else {
    stop("You need to pass either a locus object or a data.frame that has locus objects to this function")
  }
}