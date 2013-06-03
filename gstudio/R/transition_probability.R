#' Returns transition probability for offspring given one or more parents.
#' 
#' This is a quick function used in paternity testing that provides the
#'  probability of transition for a locus of any ploidy level.
#' @param off A locus from the offspring
#' @param mom A locus from one parent
#' @param dad A locus from another parent
#' @return The numeric probability of the offspring given the parents
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' off <- locus( 1:2 )
#' mom <- locus( c(1,1) )
#' dad1 <- locus( c(2,2) )
#' dad2 <- locus( c(1,2) )
#' dad3 <- locus( c(1,1) )
#' transition_probability( off, mom, dad1 )
#' transition_probability( off, mom, dad2 )
#' transition_probability( off, mom, dad3 )
transition_probability <- function( off, mom, dad ){
  if( missing( off ) | missing( mom ) | missing( dad ) )
    stop("Cannot find transition probability without offspring and both parents.")
  
  if( is.na(off) | is.na(mom) | is.na(dad) ) {
    message("missing locus, setting T(O|M,D)=1.0")
    return(1.0)
  }
  
  m <- alleles( mom )
  d <- alleles( dad )
  
  offs <- locus( cbind( rep(m, times=length(d) ), rep(d,times=length(m))), type="column", phased=FALSE )
  off <- locus( alleles(off), phased=FALSE )
  
  ret <- sum( off==offs) / length(offs)
  return( ret )
}
