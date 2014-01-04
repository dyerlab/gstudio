#' returns string representation of locus for output like genepop/heirfstat
#' 
#' This function 
#' @param x An object of type \code{locus}.
#' @param digits The number of digits that the alleles need to have
#' @return A character representation of the locus with 
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
to_fixed_locus <- function( x, digits=NULL ) {
  if( !inherits(x,"locus"))
    stop("You must pass an object of type locus to this function.")
  if( is.null(digits))
    stop("You must tell me how many digits to use to print out the fixed locus object") 
  
  if( is.na(x) ) 
    ret <- rep("0",2*digits)  
  
  else {
    ret <- character(0)
    the_alleles <- as.character(alleles(x))
    for( a in the_alleles ){
      if( nchar(a) < digits){
        buffer <- digits - nchar(a) 
        ret <- c( ret, rep("0",buffer))
      }
      else if( nchar(a) > digits){
        stop("You are asking to have fewer digits than there are in the allele which will result in data loss.  gstudio will not condone this behavior.")
      }
      ret <- c( ret, a )
    }
  }
  return( paste(ret,collapse=""))
}