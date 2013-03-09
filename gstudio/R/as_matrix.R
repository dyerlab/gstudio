

#' Turns the object \code{genetic_distance} into a matrix
#' 
#' This is a convienence function that turns a list of genetic
#'  distance estiamtes into a square matrix.
#' @param x An object of type \code{genetic_distance}
#' @param ... Ignored
#' @return A matrix with an attribe describing which metric
#'  was used to calcuate the genetic distance.
#' @method as.matrix genetic_distance
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
as.matrix.genetic_distance <- function( x, ... ) {
  ret <- x
  class(ret) <- "matrix"
  return(ret)
}











#' Overload of the as.matrix for \code{locus} objects
#' 
#' This function takes a locus and returns a matrix representation 
#'  (essentially turning it into a multivariate coding vector).
#' @param x An object of type \code{locus}
#' @param ploidy The number of alleles at an individuals locus (default=2)
#' @param alleles A set of alleles to make the matrix columns on.
#' @param drop.allele A flag indicating an allele should be dropped.
#' @param ... Ignored
#' @return Matrix representing the locus
#' @method as.matrix locus
#' @export
as.matrix.locus <- function( x, ploidy=2, alleles=NA, drop.allele=FALSE, ... ){
  ret <- 0
  if( missing(alleles) || is.na(alleles) )
    alleles <- sort(unique(as.vector(alleles(x)) ))
  
  # asking for vector of values
  if( length(x) > 1 ) {
    ret <- matrix( 0, nrow=length(x), ncol=length(alleles) )
    for( i in 1:length(x))
      ret[i,] <- as.matrix.locus( x[i], alleles=alleles, ploidy=ploidy, drop.allele ) 
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
  }

  return( ret )
}



#' Overload function for as.matrix to handle loci
#' 
#' This function looks to see if there are \code{locus} 
#'  objects in the data frame and creates a matrix from
#'  it in the appropriate fashion.
#' @param x An object of type \code{data.frame}
#' @param rownames.force A logical flag for the base::as.matrix.data.frame 
#'  function.
#' @param drop.allele A flag indicating that an allele from each locus should
#'  be dropped.
#' @param ... Optional arguments passed to base::as.matrix.data.frame
#' @return A matrix
#' @method as.matrix data.frame
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
as.matrix.data.frame <- function (x, rownames.force = NA, drop.allele=FALSE, ...)  {
  
  ret <- NULL
  locusColumns <- column_class(x, "locus", mode="index")
  
  # go default if no locus columns
  if( any(is.na( locusColumns ) )) 
    ret <- base::as.matrix.data.frame( x, rownames.force, ... )
  
  # has locus columns, return just the 
  else 
    for( col in locusColumns )
      ret <- cbind( ret, as.matrix.locus( x[,col], drop.allele=drop.allele ) )
  
  return( ret )
  
}



