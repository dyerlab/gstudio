#' Annotate auxillary data for loci
#' 
#' This will allow you to annotate additional information for a locus
#' 

column_locus_type <- function( x, locus_type ) {
  if( missing(x) || !is(x,"data.frame"))
    stop("you must pass a data.frame to the column_locus_type function.")
  
  loci <- column_class( x, "locus" )
  
  ret <- setNames(rep(NA,length(loci)), loci )
  for( locus in loci )
    ret[[locus]] <- unique( attr(x[[locus]], "locus_type") )
  
  if( missing(locus_type) )
    return( names(ret) )
  else 
    return( names(ret[ret == locus_type]) )
}