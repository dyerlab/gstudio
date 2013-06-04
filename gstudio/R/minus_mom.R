#' Subtracts maternal component to offspring genotypes
#' 
#' This function removes the female component to the offspring
#'  genotypes. It is one step in the \code{2gener} analysis. The
#'  coding of the \code{MomCol} and the \code{OffCol} headers 
#'  are specific so that this algorithm can differentiate between
#'  maternal individuals and offspring. 
#' @param x A \code{data.frame} with mother and offspring \code{locus} columns.
#' @param MomCol The name of the column indicating maternal ID.  All offspring
#'  from a mother MUST have the same MomCol value.  
#' @param OffCol The name of the column indicating the offspring ID number.  It is
#'  required that maternal individuals have OffCol="0" indicating that this is the 
#'  mother of those offspring.
#' @return A \code{data.frame} object of only the offspring after removing the
#'  contribution of each maternal individual.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
#' @examples
#' AA <- locus( c("A","A") )
#' AB <- locus( c("A","B") )
#' BB <- locus( c("B","B") )
#' AC <- locus( c("A","C") )
#' AD <- locus( c("A","D") )
#' BC <- locus( c("B","C") )
#' BD <- locus( c("B","D") )
#' CC <- locus( c("C","C") )
#' CD <- locus( c("C","D") )
#' DD <- locus( c("D","D") ) 
#' loci <- c(AA,AB,AC,AD,BB,BC,BD,CD,CD,DD) 
#' offID <- c(0 ,1,2 , 3, 0, 1, 2,1, 0, 2 )
#' momID <- c(rep("A",4), rep("B",3), rep("C",3))
#' df <- data.frame( ID=factor(momID), OffID=factor(offID), TPI=loci )
#' minus_mom( df )
minus_mom <- function( x, MomCol="ID", OffCol="OffID"  )  {
  
  
  # x is data.frame in 
  if( is(x,"data.frame") & (MomCol %in% names(x)) & (OffCol %in% names(x) ) ) {
    locus_names <- column_class(x,"locus")
    
    if( length( locus_names) == 0 )
      stop("You need to have some loci to use this function.")
  
    moms <- x[ x[[OffCol]]=="0",]
    ret <- x[ x[[OffCol]]!="0",]
    N <- dim(ret)[1]
    
    for( i in 1:N ){
      off <- ret[i,]
      momNum <- off[[MomCol]]
      mom <- moms[ moms[[MomCol]] == momNum , ]
      
      for( locus in locus_names )
        off[[locus]] <- off[[locus]] - mom[[locus]]
      ret[i,] <- off
    }
    
  }

  else
    stop("Cannot subtract maternal contributions given the data you provided.")


  
  return(ret)
}




