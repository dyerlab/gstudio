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
#' @param relabel A flag (default TRUE) that relabels individuals after
#'  they have been moved between populations.  Used for testing only.
#' @return A \code{data.frame} with all the data the same except for
#'  the shuffled stratum.
#' @note This is a simple transition matrix approach to migration.  The
#'  migration rate times the population size MUST result in at least 
#'  a whole number or else no migrants are produced.  It uses \code{round()}
#'  to find the whole number of individuals to migrate.  This means that 
#'  if you have a migration rate that has less than one individual it 
#'  will NEVER occur, not that it will occur at a less than one individual
#'  per generation frequency.  You must adjust your migration rates 
#'  directly for that (e.g., the user has the entire power, not the 
#'  this function).
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
migrate <- function( data, stratum="Population", m=0.1, relabel=TRUE){
  if( !is(data,"data.frame") )
    stop("You must pass a data.frame object to the migrate() function.")
  if( !(stratum %in% names(data) ) )
    stop("You need to indicate the stratum to use for migration.")
  
  pops <- partition( data, stratum )
  strata <- names(pops)
  if( !is(m,"matrix") ){
    K <- length(strata)
    mat <- matrix( m, K, K)
    rownames(mat) <- colnames(mat) <- strata
    diag(mat) <- 0
    diag(mat) <- 1 - rowSums( mat )
    m <- mat
  }
  
  # catch errors
  if( any(m > 1.0))
    stop("You must specify the migration rate as a rate (e.g., they must be <= 1.0)")
  
  
  # figure out how to allocate them
  n <- as.numeric(table(data[[stratum]]))
  to_move <- round(n * m)
  
  # do the off-diagonals from(row) to(col) movements
  newpops <- vector('list',K)
  names(newpops) <- strata
  for( i in 1:K){
    for( j in 1:K){
      moving <- to_move[i,j]
      all_indx <- seq(1,nrow(pops[[i]]))
      idx <- sample( all_indx, size=moving, replace=FALSE)
      df <- pops[[i]][idx,]
      
      if( is.null(newpops[[j]]))
        newpops[[j]] <- df
      else
        newpops[[j]] <- rbind( newpops[[j]], df)    
      
      idx <- setdiff( all_indx,idx)
      pops[[i]] <- pops[[i]][idx,]
      
    }
  }
  
  ret <- data.frame()
  for( pop in strata){
    df <- newpops[[pop]]
    if( relabel )
      df[[stratum]] <- pop
    ret <- rbind(ret,df) 
  }
  
  rownames(ret) <- seq(1,nrow(ret))
  return( ret )
}
