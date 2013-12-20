#' This function returns a data frame that has moved migrants
#' 
#' This is a general function that moves migrants among strata
#'  according to either a constant migration rate or a 
#'  set of migration rates defined by a migration matrix. All
#'  this does is adjust the stratum labels of individuals, 
#'  no mating is conducted.
#' @param data A \code{data.frame} object with at least a stratum
#'  column.  All other columns in the data frame are left untouched
#' @param stratum The column designating stratum. 
#' @param m Either a rate of migration as a numeric OR a migration 
#'  matrix whose columns and row names are the same as those in the
#'  passed stratum column.
#' @return A \code{data.frame} with all the data the same except for
#'  the shuffled stratum.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
migrate <- function( data, stratum="Population", m=0.01){
  if( !is(data,"data.frame") )
    stop("You must pass a data.frame object to the migrate() function.")
  if( !(stratum %in% names(data) ) )
    stop("You need to indicate the stratum to use for migration.")
  
  strata <- unique( as.character( data[[stratum]] ) )
  if( !is(m,"matrix") ){
    
  }
}
