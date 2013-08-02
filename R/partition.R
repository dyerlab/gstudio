#' Subdivide , into a list of substrata
#' 
#' This function allows you to take a single \code{Population} object
#'  into a list of \code{Population} objects, one for each stratum.
#' @param x Any object that can be column indexed by the function \code{names}
#' @param stratum The column name of the stratum to partition on (default='Population').
#' @return A partitioned list of objects, indexed by name
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#'  AA <- locus( c("A","A") )
#'  AB <- locus( c("A","B") )
#'  BB <- locus( c("B","B") )
#'  locus <- c(AA,AA,AA,AA,BB,BB,BB,AB,AB,AA)
#'  Population <- c(rep("Pop-A",5),rep("Pop-B",5))
#'  locus2 <- c(AB,BB,AA,BB,BB,AB,AB,AA,AA,BB)
#'  df <- data.frame( Population, TPI=locus, PGM=locus2 )
#'  partition(df, stratum="Population")
partition <- function( x, stratum="Population" ){

  if( !(stratum %in% names(x) ) )
    stop(paste("Cannot find '",stratum,"' in the column names of your Population."))
  
  ret <- list()
  lvls <- sort( unique( x[[stratum]]) )
  
  for( lvl in lvls ) 
    ret[[lvl]] <- x[ x[[stratum]]==lvl, ]
  
  return( ret )
}
