#' This function finds a set of potential parents from an offspring.
#' 
#' This is a general exclusion finder for a single parent. For dual
#'  parent exclusion, you can use the \code{paternity()} function.
#' @param df A \code{data.frame} of individuals adults individuals.  This will
#'  be the population from which the identification of potential parentage will
#'  be examined..
#' @param ID The adult "ID" column designator (default="ID").  All adults 
#'  have a unique ID designation.  This will be how each adult is identified.  
#' @param OffID This is a non-zero number for each offspring.  The combination of 
#'  ID and OffID should be able to be used to identify each offspring uniquely.  All
#'  adults have OffID=0, by definition.
#' @return A \code{data.frame} with the following columns.  ID = the offspring id, 
#'  OffID = the offspring OffID (these two will identify the offspring uniquely), 
#'  ParentID = the putative parent identified, T = the multilocus transition 
#'  probability associated with the offspring and parent.
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
parent_finder <- function( df, ID="ID", OffID="OffID"){
  
  if( !(ID %in% names(df)))
    stop("You need to supply the ID column for the data.frame.")
  if( !(OffID %in% names(df)))
    stop("You need to supply the OffID column designation for the data.frame")
  
  offs <- df[ df[[OffID]]!=0,]
  adults <- df[ df[[OffID]]==0,]
  
  ret <- data.frame( ID=NULL, OffID=NULL, ParentID=NULL, T=NULL)
  
  for( i in 1:nrow(offs)){
    for( j in 1:nrow(adults)) {
      t <- transition_probability( offs[i,], adults[j,], multilocus=TRUE )
      if( t > 0 ) {
        ret <- rbind( ret, data.frame(ID=offs[[ID]][i], OffID=offs[[OffID]][i], ParentID=adults[[ID]][j], T=t ))
      }
    }
  }
  ret <- ret[ order( ret$ID, ret$OffID, -ret$T),]
  return( ret )
}