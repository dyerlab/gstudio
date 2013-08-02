#' Find columns of data in a data.frame for a type of class
#' 
#' This convienence function finds the data of a specified
#'  type in the passed \code{data.frame}.
#' @param x An object of \code{data.frame} type.
#' @param class The type of class to search for.  If this is omitted then
#'  the classes of each column will be returned.
#' @param mode How you would like the column references to be used.  Current
#'  values are:
#'    \itemize{
#'      \item{label}{The name of the column label with the specified class.}
#'      \item{index}{The numerical index of the column with specified class.}
#'    }
#' @return A list of labels or indices indicating where
#'  columns of the querried type exist or a list of all column classes.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu} 
#' @examples
#' locus <- c( locus(1:2), locus(c(1,1)), locus(c(2,2)), locus(2:3) )
#' pop <- factor( c(rep("A",2), rep("B",2)))
#' df <- data.frame( Population=pop, X=runif(4), TPI=locus )
#' df
#' column_class(df,"factor")
#' column_class(df,"numeric")
#' column_class(df, "locus" )
#' column_class(df,"locus",mode="index")
column_class <- function( x, class, mode=c("label","index")[1] ) {
  if( !inherits(x,"data.frame") )
    stop( paste("This function works on objects inherited from data.frame objects, you passed a ",class(x),sep="") )
  
  if( !(mode %in% c("label","index") ) )
    stop( paste("The mode for this class must be either 'label' or 'index'") )
  
  if( missing( class) )
    return( apply( x, 2, class ))
  
  
  nms <- names(x)
  cls <- rep( NA, length(nms) )
  for(i in 1:length(nms) ){
    cls[i] <- class( x[,i] )
  }
  if( !(class %in% cls ) )
    return( NA )

  if( mode=="label" ){
    return( nms[ cls==class ] )
  }
  else if( mode=="index"){
    return( (1:length(cls))[ cls==class ] )
  }
  else
    return(NA)
}


