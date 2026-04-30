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
#' @note This uses a multinomial draw to allocate each population's individuals
#'  among destinations, which guarantees population sizes are exactly preserved
#'  each generation.  Migration is stochastic: a rate of \code{m} means each
#'  individual independently has probability \code{m} of leaving (split equally
#'  among non-self populations in the island model, among neighbours in stepping
#'  stone models).  Very small \code{m * N} values will frequently round to zero
#'  migrants; adjust your rate or population size accordingly.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
migrate <- function( data, stratum="Population", m=0.1, relabel=TRUE){
  if( !is(data,"data.frame") )
    stop("You must pass a data.frame object to the migrate() function.")
  if( !(stratum %in% names(data) ) )
    stop("You need to indicate the stratum to use for migration.")
  
  pops <- partition( data, stratum )
  strata <- names(pops)
  K <- length(strata)
  if( !is(m,"matrix") ){
    mat <- matrix( m, K, K)
    rownames(mat) <- colnames(mat) <- strata
    diag(mat) <- 0
    diag(mat) <- 1 - rowSums( mat )
    m <- mat
  }

  # align named matrix rows/cols to the sorted strata order
  if( !is.null(rownames(m)) )
    m <- m[strata, strata]

  # catch errors
  if( any(m > 1.0))
    stop("You must specify the migration rate as a rate (e.g., they must be <= 1.0)")

  # figure out how to allocate them
  # rmultinom ensures each row's moves sum exactly to n[i], which round() cannot
  # guarantee and can cause crashes or silent individual loss
  n <- as.integer(table(data[[stratum]])[strata])
  to_move <- matrix(0L, K, K)
  for( i in seq_len(K) )
    to_move[i, ] <- as.integer(stats::rmultinom(1L, n[i], m[i, ]))

  # do the off-diagonals from(row) to(col) movements
  newpops <- vector('list',K)
  names(newpops) <- strata
  for( i in seq_len(K) ){
    for( j in seq_len(K) ){
      moving <- to_move[i,j]
      all_indx <- seq_len(nrow(pops[[i]]))
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
  for( pop in strata ){
    df <- newpops[[pop]]
    if( relabel )
      df[[stratum]] <- pop
    ret <- rbind(ret,df) 
  }
  
  rownames(ret) <- seq(1,nrow(ret))
  return( ret )
}
