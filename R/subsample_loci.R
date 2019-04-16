#' Generate subset of data with random sample of loci.
#' 
#' This function is to assist in estimating statistical strength of your
#'   inferences by subsampling the loci you have.  
#' @param x A \code{data.frame} that has \code{Locus} objects in it.
#' @param numLoci The number of loci you wnat to sample (must be \code{length(column_class("locus"))})
#' @return A new \code{data.frame} with a subset of loci.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' data(arapat)
#' sub <- subsample_loci( arapat, numLoci = 4 )
#' summary( sub )

subsample_loci <- function( x, numLoci ) {
  column_names <- column_class(x)
  locus_names <- names(x)[ column_names == "locus" ]
  
  if( length(locus_names) <= numLoci ) {
    stop("You cannot ask for a random sample of loci whose size is larger than the number of loci in the data.frame.  What am I supposed to do, make up data?  Come on!")
  }

  to_keep <- c( names(x)[ column_names != "locus"],
                sort( sample( locus_names, size=numLoci) ) )
  return( x[,to_keep] )
}