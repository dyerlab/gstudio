#' Determines of the indicated parent can possibly be the parent of the offspring
#' 
#' This function takes a \code{data.frame} full of parents and offspring
#'  and checks to see if the offspring are actually assigned to a compatible
#'  adult.
#' @param df A \code{data.frame} with identification and locus columns.
#' @param AdultID The column header for id of the adults. All adults have unique
#'  identification numbers and all offspring have the the same number as
#'  the maternal/paternal indiviudal from which they were sampled or are being
#'  compared.  Default = "ID"
#' @param OffID A column indicating offspring identification numbers.  By default
#'  all adults have OffID=0 (this is how I tell if they are really adults and not
#'  offspring), and all offspring have OffID != 0. Default = "OffID"
#' @param verbose Print out mismatched parent/offspring pairs.
#' @return The \code{data.frame} with a new column, Is.Parent with 
#'  values of TRUE/FALSE/NA (the NA is for adults).
#' @export
bad_parents <- function( df, AdultID="ID", OffID="OffID", verbose=FALSE) {
  if( !length(column_class(df,"locus")) ) 
    stop("No need to try to identify bad parents when you do not have loci in the data")
  if( !(AdultID %in% names(df) ) )
    stop("You need to have the AdultID actually in your data.frame for this function to work...")
  if( !(OffID %in% names(df) ) )
    stop("You need to have the OffID actually in your data.frame for this function to work...")
  
  if( !inherits(df[[OffID]], "character"))
    df[[OffID]] <- as.character( df[[OffID]])
  
  df$Is.Parent <- NA
  loci <- column_class(df,"locus")
  for( i in 1:nrow(df) ) {
    ind <- df[i,]
    if( !is.na(ind[[AdultID]]) & ind[[OffID]] != "0" & !is.na(ind[[AdultID]])) {
      mom <- df[ df[[AdultID]] == ind[[AdultID]] & df$OffID=="0",]
      if( nrow(mom) > 0 ){
        t <- transition_probability(ind,mom)
        if( verbose &  t == 0 )
          print(rbind(mom,ind))
        df$Is.Parent[i] <- ( t>0 )
      }
    }
  }
  
  return( df )
}