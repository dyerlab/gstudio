#' Returns the passed items as multivariate data
#' 
#' This function will provide the alleles within a \code{locus} object 
#'  for either a single locus or for a vector of loci
#' @param x A object to convert.
#' @param ploidy The number of alleles at an individuals locus (default=2)
#' @param alleles A set of alleles to make the matrix columns on.
#' @param drop.allele A flag indicating an allele should be dropped.
#' @param leave.as.na A flag to re-insert NA values into multivariate data where missing
#'  genotypes were encountered (Default=FALSE) if not, zeros are used.
#' @param ... Ignored
#' @return The matrix representation of \code{x}.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
to_mv <- function( x, ploidy, alleles, drop.allele=FALSE, leave.as.na=FALSE, ... ) { 
  UseMethod( "to_mv" )
}
 

#' @return Numerical value of x
#' @method to_mv default
#' @export
#' @rdname to_mv
to_mv.default <- function( x, ... ) {
  return( as.numeric( x ) )
}


#' @return Matrix representing the locus
#' @method to_mv locus
#' @export
#' @rdname to_mv
to_mv.locus <- function( x, ploidy=2, alleles=NA, drop.allele=FALSE, leave.as.na=FALSE, ... ){
  ret <- 0
  if( missing(alleles) || all(is.na(alleles)) )
    alleles <- sort(unique(as.vector(alleles(x)) ))
  
  # asking for vector of values
  if( length(x) > 1 ) {
    ret <- matrix( 0, nrow=length(x), ncol=length(alleles) )
    for( i in 1:length(x))
      ret[i,] <- to_mv.locus( x[i], alleles=alleles, ploidy=ploidy, drop.allele=drop.allele, leave.as.na=leave.as.na ) 
    colnames(ret) <- alleles
    if( drop.allele & ncol(ret)>1)
      ret <- ret[,1:(ncol(ret)-1)]
  }
  
  # asking for single locus
  else { 
    all.alleles <- alleles(x)
    ret <- list()
    ret[alleles] <- 0
    for( allele in all.alleles) 
      ret[[allele]] <- ret[[allele]] + 1
    
    ret <- unlist( ret )/ploidy
    
    if( any(is.na(x)) && leave.as.na)
      ret <- rep(NA,length(ret))
  }

  return( ret )
}





#' @return A matrix
#' @method to_mv data.frame
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @rdname to_mv
to_mv.data.frame <- function (x, ploidy=2, alleles=NA, drop.allele=FALSE, leave.as.na=FALSE, ...)  {
  cols <- column_class(x, "locus", mode="index")
  
  if(any(is.na(cols)))
    stop("Cannot make loci to mv if there are no loci.")
  
  df <- x[,cols]  
  ret <- NULL
  for( col in cols )
    ret <- cbind( ret, to_mv.locus( x[,col], ploidy=ploidy, alleles=alleles, drop.allele=drop.allele, leave.as.na=leave.as.na ) )    
  return( ret )
}



