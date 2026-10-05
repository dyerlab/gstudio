#' This function takes a data set and a group set and returns centroid variances
#' 
#' This is a convience function for a reduce and distance operation based upon
#'  multivariate predictor variables.
#' @param x The NxP matrix of raw data.
#' @param grouping The Nx1 vector of grouping factors.
#' @return A \code{data.frame} with one row per group: \code{Stratum} and
#'   \code{variance}, the sum of the within-stratum variances.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
#' @examples
#' data(arapat)
#' mv <- to_mv(arapat)
#' cv <- centroid_variance(mv, arapat$Population)
#' head(cv)
centroid_variance <- function( x, grouping ){
  
  if( !is(grouping, "factor"))
    grouping <- factor( grouping )
  N <- dim(x)[1]
  
  if( length(grouping) != N )
    stop("Your grouping and predictor variables should ',
         'have the same number of rows....  Come on now.")
  
  grps <- levels( grouping) 
  K <- length(grps)
  ret <- rep(NA,K)
  names(ret) <- grps

  
  for( i in 1:K ){
    xp <- x[ grouping == grps[i],]
    ret[i] <- sum(diag(cov(xp)))
  }

  data.frame( Stratum = grps, variance = unname(ret), stringsAsFactors = FALSE )
} 