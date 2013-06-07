#' Estimates allelic diversity
#' 
#' This function takes a single \code{locus} object or a \code{data.frame} object
#'  and estimates allelic diversity as either the number of alleles, the effective
#'  number of alleles, or the number of alleles observed at a frequency of at least
#'  five percent.
#' @param x A \code{locus} object or a \code{data.frame} with some loci in it
#' @param mode The kind of diversity to estimate.  Current options include 'A' the 
#'  total number of alleles, 'Ae' the effective number of alleles, and 'A95' the 
#'  number of alleles with a frequency of at least five percent.
#' @return Numeric value for diversity
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#' locus <- c( locus(1:2), locus(c(1,1)), locus(c(2,2)), locus(2:3) )
#' locus2 <- c( locus(1:2), locus(c(1,1)), locus(c(2,2)), locus(2:3) )
#' df <- data.frame( locus, locus2 )
#' allelic_diversity(df,mode="A")
#' allelic_diversity(df,mode="Ae")
allelic_diversity <- function( x, mode=c("A","Ae","A95")[1] ) {
  
  if( !(mode %in% c("A","Ae","A95" ) ) )
    stop("The 'mode' you passed for allelic_diversity() is not recognized.")
  
  if( is(x,"locus") ) {
    
    a <- NA
    if( mode == "A" ) {
      a <- A(x) 
    }
    else if( mode == "Ae" ) {
      a <- Ae(x) 
    }
    else if( mode == "A95") {
      a <- A(x, min_freq=0.05) 
    }
    names(a) <- mode
    return( a )
  }
  
  else if( is(x,"data.frame") ) { 
    cols <- column_class(x,"locus")
    ret <- list()
    for( col in cols )
      ret[[col]] <- allelic_diversity( x[[col]], mode )
    ret <- as.numeric( ret )
    names(ret) <- cols
    return( ret )
  }
  else
    stop("data type 'x' passed to allelic_diversity() not recognized.")
}

