#' Convert to DFDist format
#' 
#' This converts a \code{data.frame} object with loci into a 
#'   format suitable for FDist, DFDist, & DetSel
#' @param x A \code{data.frame} with some \code{locus} objects in it.
#' @param stratum The name of the 'Population' column in the data 
#'   (default="Population").
#' @param verbose A flag to print locus names as you go (default=FALSE)
#' @return A text object with data formatted for output.
#' @export
to_dfdist <- function( x, stratum="Population", verbose=FALSE){
  if( !is(x,"data.frame"))
    stop("to_dfdist needs a data.frame to work with.")
  if( !( stratum %in% names( x )))
    stop("Please indicate the 'Population' column in your data frame.")
  
  loci <- column_class(x,"locus")
  if( is.na(loci) || length(loci)==0 )
    stop("You need to pass some data that actually has some locus objects in it...")
  
  # make sure the stratum is a factor
  if( !is(x[[stratum]], "factor"))
      x[[stratum]] <- factor( x[[stratum]] )
  
  strata <- levels( x[[stratum]])
  K <- length( strata )
  if( K < 2 )
    stop("You must have at least 2 stratum...")
  
  ret <- c("0", K, length(loci),"" )
  for( locus in loci ){
    if( verbose )
      message(locus, " ", appendLF = FALSE)
    cts <- allele_counts( x, locus, stratum )
    cts <- cts[,2:ncol(cts)] # remove Stratum column
    ret <- c( ret, ncol(cts) )
    rows <- as.vector( apply( cts, 1, function(x) return(paste(x,collapse="     "))) )
    ret <- c( ret, rows, "" )
  }
  
  
  
  return( paste(ret,collapse="\n" ) )
  
}