#' Subtracts maternal component to offspring genotypes
#' 
#' This function removes the female component to the offspring
#'  genotypes. It is one step in the \code{2gener} analysis.
#' @param mother A \code{locus} object for the mother.
#' @param offspring A \code{locus} object for the offspring (from
#'  the same locus).
#' @return A \code{locus} object of the offspring after removing the
#'  contribution of the maternal individual.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
#' @examples
#' offs <- c(locus( 1:2 ), locus( c(1,1) ), locus( c(1,2) ))
#' mom <- locus( c(1,1) )
#' minus_mom( mom, offs )
#' 
minus_mom <- function( mother, offspring )  {
  
  if( !is(mother,"locus") | !is(offspring,"locus"))
    stop("Cannot subtract maternal contributions to non-locus objects.")
  

  ret <- unlist(lapply( offspring, function(off,mom) return( off - mom), mom=mother))
  
  
  
  
  return(ret)
}




