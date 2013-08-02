#' Permute for confidence interval
#' 
#' This function takes some data and does perumations on it and passes
#'  it along to the indicated function.  There are options for permuting
#'  genotypes or permuting alleles.  Make sure you know the difference.
#' @param x The \code{locus} data being used.
#' @param stratum An optional argument if passed, permutes \code{x} among 
#'  strata and gives the results to the function.
#' @param allele_perm A flag (default=TRUE) to permute alleles in the
#'  creating the new data set.
#' @param nperm The number of perms to use (default 99).
#' @param FUN the function to pass the permuted data to.
#' @param replace A flag indicating if randomization should be with replacement (default FALSE)
#' @param ... Passed on as arguments to \code{FUN}.
#' @return A vector of permuted response values from the function.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
permute_ci <- function( x, stratum=NULL, allele_perm=TRUE, nperm=99, FUN=NULL, replace=FALSE, ... ) {
  
  if( is.null(FUN) )
    stop("Cannot run function permute_ci() without a function to pass the data to...")
  else
    FUN <- match.fun( FUN )
  
  if( !is(x,"locus"))
    stop("This function only works on object of type 'locus'.")
  
  if( !is.null( stratum ) & length(stratum) != length(x) )
    stop("If you are going to pass a stratum variable to permute_ci() it needs to be the same length as the genetic data you are passing.")

  if( any( is.na(x) ) )
    warning( "Some of your data is 'missing' and as such will be permuted as normal.  Make sure this is what you intend to do.")
  
  ret <- numeric(nperm)
  for( i in 1:nperm){
    
    # permute strata and do function
    if( !is.null(stratum) ) {
      stratum <- sample( stratum, size=length(stratum) )
      ret[i] <- eval( as.call( list(FUN,x,stratum,...)))
    }
    
    # must be individual genotype-only function
    else {
      
      if( !allele_perm )
        xp <- sample(x, size=length(x), replace=replace)
      
      else {
        a <- alleles( x )
        xp <- matrix( sample( a, size=nrow(a)*ncol(a), replace=replace ), ncol=ncol(a))
        xp <- locus( xp, type="column" )
      }
      
      ret[i] <- eval( as.call(list(FUN,xp,...)))
    }
    
  }
  
  ret <- unlist(ret)
  return( ret )  
}


