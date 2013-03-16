#' Returns the alleles in a \code{locus} object
#' 
#' This function will provide the alelles within a \code{locus} object 
#'  for either a single locus or for a vector of loci
#' @param x A \code{locus} object (single or vector)
#' @return A matrix of alleles.  If \code{x} is a vector then the result
#'  will be represented by rows.
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
alleles <- function( x ) { 
  UseMethod( "alleles" )
}


#' @return Names from passed vector
#' @method alleles default
#' @export
#' @rdname alleles
alleles.default <- function( x ) {
  return( names( x ) )
}



#' @return Either a \code{matrix} or \code{vector} of alleles depending
#'  upon what was passed to the function.
#' @method alleles locus
#' @export
#' @rdname alleles
alleles.locus <- function ( x ) {
  ret <- NULL

  # catch all missing 

    if( length(x) > 1 ) {
      lst <- lapply( x, alleles.locus )
      ncol <- max( unlist( lapply(lst,length)))
      if( ncol ){
        ret <- matrix(NA,nrow=length(lst), ncol=ncol)
        for( i in 1:length(lst)){
          rep <- lst[[i]]
          if( length(rep) > 0)
            ret[i,1:length(rep)] <- lst[[i]]
        }
      }
    }  
    else if( length(x) == 1 ) {  
        ret <- unlist(strsplit(x,":"))
    }
      

  
  return( ret )  
}






