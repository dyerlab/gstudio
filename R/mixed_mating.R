#' Produce the next generation but allow for some level of selfing.
#' 
#' This function is a bit of a wrapper around the \code{mate} function in that
#'  it allows you to specify the inbreeding parameter F (the fraction of indiviuals
#'  that produce offspring via selfing).
#' @param data A \code{data.frame} containing indiviudals with objects of type \code{locus}
#' @param N The total number of individuals in the next generation to be produced by the mating
#'  of each individual.  If empty, it will default to 1 offpspring per pair.
#' @param F The fraction of adults you want to be selected to produce selfed offspring, the 
#'  default is 0 and this will produce a randomly mating population just like if you called
#'  \code{mate()}.
#' @return A \code{data.frame} that has new individuals that are the result of mixed mating
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
mixed_mating <- function( data, N=1, F=0) {
  if( F < 0.0 | F > 1.0) 
    stop("F must be bound on [0,1] at this time.")
  if( !is(data,'data.frame'))
    stop("You must pass a data.frame object to use this function.")
  if( is.na( column_class(data,"locus")) )
    stop("Please pass at least one locus object to this function...")
  
  # Catch if F=0 ~ No selfing
  if( F==0 )
    ret <- mate( data, data, N )
  
  # some degree of selfing
  else {
    
    Nselfed <- F*nrow(data)
    Noutcross <- nrow(data) - Nselfed

    # index to selfing
    if( Nselfed ) {
      idx <- sample( 1:nrow(data), size=Nselfed, replace=TRUE)
      ret <- mate( data[ idx,],data[idx,], N)      
    }
    
    # index to random mate pairs
    if( Noutcross ){
      idx1 <- sample( 1:nrow(data), size=Noutcross, replace=TRUE )
      idx2 <- sample( 1:nrow(data), size=Noutcross, replace=TRUE )
      ret <- rbind( ret, mate( data[idx1,], data[idx2,], N ) )       
    }
    
    # fix up the ID column
    if( "ID" %in% names( ret ) )
      ret$ID <- 1:nrow(ret) 
  }
  
  return( ret )
}
