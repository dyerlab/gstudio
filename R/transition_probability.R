#' Returns transition probability for offspring given one or more parents.
#' 
#' This is a quick function used in paternity testing that provides the
#'  probability of transition for a locus of any ploidy level.
#' @param off A locus from the offspring
#' @param mom A locus from one parent
#' @param dad A locus from another parent
#' @param multilocus A flag indicating that a single multilocus estiamte
#'  of the transition probability should be passed (the default action).  
#'  If \code{FALSE}, the results will be returned as a vector of individual
#'  locus values.
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
transition_probability <- function( off, mom, dad , multilocus=TRUE){
  
  if( class(off) != class(mom) )
    stop("You need to pass either a set of 'locus' or 'data.frame' objects to this function.")
  
  ret <- NA
  
  # multilocus types
  if( is(off,'data.frame') ){
    loci <- column_class(off,'locus' )
    ret <- list()  
    
    for( locus in loci ) {
      oloc <- off[1,][[locus]]
      mloc <- mom[1,][[locus]]
      if( missing(dad) )
        ret[[locus]] <- transition_probability( oloc,mloc )
      else
        ret[[locus]] <- transition_probability( oloc,mloc,dad[1,][[locus]])            
    }
    
    ret <- unlist( ret )    
    if( multilocus ) {
      ret <- ret[ !is.na(ret) ]
      ret <- as.numeric( cumprod( ret ))[length(ret)]
    }
  }
  
  # single locus estimates
  else if( is( off, "locus") ) {  
    
    # not missing
    if( !is.na(mom) & !is.na(off) ) {
      
      # one parent
      if( missing(dad) ){
        ret <- ifelse( is_heterozygote(mom) , 0.5, 1.0 )
        if( is_heterozygote(off) ) 
          ret <- ret/2
        ret <- ret * length(intersect( alleles(mom), alleles(off) ) )
      }
      
      # two parent
      else {
        if( !is.na(dad)) {
          m <- alleles( mom )
          d <- alleles( dad )
          
          offs <- locus( cbind( rep(m, times=length(d) ), rep(d,each=length(m))), type="column", phased=FALSE )
          off <- locus( alleles(off), phased=FALSE )
          
          ret <- sum( off==offs ) / length(offs)                 
        }
      }
    }
  }
  return( ret )  
  
}
