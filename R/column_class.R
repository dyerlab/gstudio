#' Find columns of data in a data.frame for a type of class
#' 
#' This convienence function finds the data of a specified
#'  type in the passed \code{data.frame}.
#' @param x An object of \code{data.frame} type.
#' @param class The type of class to search for.  If this is omitted then
#'  the classes of each column will be returned.
#' @param mode How you would like the column references to be used.  Current
#'  values are:
#'    \describe{
#'      \item{label}{The name of the column label with the specified class.}
#'      \item{index}{The numerical index of the column with specified class.}
#'    }
#' @return A list of labels or indicies indicating where
#'  columns of the queried type exist or a list of all column classes.
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
column_class <- function( x, class=NULL, mode=c("label","index")[1] ) {
  if( !inherits(x,"data.frame") )
    stop( paste("This function works on objects inherited from data.frame objects, you passed a ",
                class(x),sep="") )
  
  if( !(mode %in% c("label","index") ) )
    stop( paste("The mode for this class must be either 'label' or 'index'") )
  
  if( is.null( class) ) {
    labels <- names(x)
    ret <- rep("",length(labels) )
    for( i in 1:length(labels) ){
      val <- class( x[[labels[i]]])
      ret[i] <- ifelse( length(val)>1, val[-1], val )
    }
    return( ret )
  }
  
  nms <- names(x)
  cls <- rep( NA, length(nms) )
  
  if( is(x,"tbl") ) { 
    for(i in 1:length(nms) ){
      # catch for column class
      val <- class( pull( x, nms[i]) )
      cls[i] <- ifelse( length(val)>1, val[-1], val )
    }
  } 
  else if( is(x,"data.frame")) { 
    for(i in 1:length(nms) ){
      # catch for column class
      val <- class( x[,i])
      cls[i] <- ifelse( length(val)>1, val[-1], val )
    }
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


