#' Default constructor for locus
#' 
#' The main constructor for a \code{locus} object.  This can take either a single set of alleles
#'  or a matrix of alleles.
#' @param x The vector of alleles for a locus.  For a single locus this can be either a numeric 
#'  or character set of variables.  For several loci, this should be a matrix of values with each
#'  row representing a different locus.
#' @param is.snp.minor A logical flag indicating that the values in \code{x} are 0, 1, or 2 indicating 
#'  number of 'minor' alleles at the SNP locus.  Major alleles will be encoded as 'A' whereas minor 
#'  alleles will be indicated by allele 'B'.  The default is \code{FALSE}.
#' @param is.zyme A logical flag that indicates that the values in \code{x} are zyme-like genotypes 
#'  (e.g., 12, 22, 33).  This will also split larger allele counts (e.g., 122144 is equivalent to 
#'  c(122,144)).  The default is \code{FALSE}
#' @param is.phased A logical flag indicating that the order of alleles in the genotype are important 
#'  (e.g., they have been phased).  The default is \code{FALSE}.
#' @param is.separated A logical flag indicating that the alleles being passed are already separated 
#'  by a colon value
#' @param \dots Ignored at this time.
#' @export
#' @return An object of one or more \code{locus} entities.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' 
#' loc1 <- locus( c(1,1) )
#' loc1
#' loc2 <- locus( 1:2 ) 
#' loc2 
#' 
locus <- function( x=character(0), is.snp.minor=FALSE, is.zyme=FALSE, is.phased=FALSE, 
                   is.separated=FALSE,... ) {
  
  if( is.separated && is(x,"character") && length(x) > 1 ) 
    x <- matrix(x)
  
  if( is(x,"matrix") || is(x,"data.frame")) 
    ret <- apply( x, 1, locus, is.snp.minor=is.snp.minor, is.zyme=is.zyme, is.phased=is.phased )
  
  else { 
    # x is missing or NA
    if( missing(x) || is.na(x) )
      alleles <- character(0)
    
    # snp minor
    if( is.snp.minor ) {
      if( x == 0 )
        alleles <- c("A","A")
      else if( x==1 )
        alleles <- c("A","B")
      else if( x==2 )
        alleles <- c("B","B")
      else
        alleles <- character(0)
    }
    else if( !is(x,"character") )
      alleles <- as.character(x)
    else
      alleles <- x

    if( is.zyme ){
      N <- nchar(alleles)
      n <- N/2
      l <- substr(alleles,1,n)
      r <- substr(alleles,(n+1),N)
      alleles <- c(l,r)
    }
    
    if( !is.phased ) {
      if( is.separated )
        alleles <- strsplit(x,split=":")[[1]]
      alleles <- sort(alleles)
    }
    
    ret <- paste( alleles, collapse=":")    

  }
  class(ret) <- "locus"
  ret
}






##########################################################################
#                                                                        #
#                       Extending S3 Methods                             #
#                                                                        #
##########################################################################








#' Converts locus to a list
#' 
#' This converts a \code{locus} object into a \code{list} so that
#'  you can use \code{locus} objects in a column of a \code{list}.
#' @param x An object of type \code{locus}.  This can be either a single genotype 
#'  (a rare case) or a vector of genotypes (preferred). 
#' @param \dots Additional objects that are passed to \code{as.data.frame.vector}.
#' @return A \code{list} object.
#' @method as.list locus
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' 
#' loc1 <- c( locus(1:2), locus(1:2), locus( c(1,1) ) )
#' df <- as.list( ID=1:3, NAMED_LOCUS=loc1, loc1 )
#' df
#'
as.list.locus <- function( x, ... ) {
  ret <- list()
  for( i in 1:length( x )) { 
    ret[[i]] <- x[i]
  }
  ret
}



#' As operator for locus
#' 
#' This takes several things and shoves it into the constructor
#' @param x An object that is to be truned into a \code{locus}.
#' @return An object of type \code{locus}
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @seealso \code{\link{locus}}
#' @examples
#' 
#' lst <- list( "A", "B" )
#' as.locus( lst )
#' vec <- 1:2
#' as.locus( lst )
#' chr <- "A"
#' as.locus( chr )
#' chr.sep <- "A:A"
#' as.locus( chr )
#' 
as.locus <- function( x ) {
  return( locus(x) )
}



#' Concatinate \code{locus} objects
#' 
#' An overload of the \code{c} function for \code{locus} objects.
#' @param \dots The \code{locus} objects to be concatenated.
#' @param recursive A flag to do the concatenation recursively.
#' @return A vector of \code{locus} objects
#' @method c locus
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' 
#' loci <- c( locus(1:2), locus(1:2), locus( c(1,1) ) )
#' loci
#' 
c.locus <- function(..., recursive = FALSE) {
  dots <- list(...)
  classes <- rep("locus", length(dots))
  res <- structure(unlist(dots, recursive = recursive), class = classes)
  class(res) <- "locus"
  res
}




#' Overload of \code{print} function for \code{locus} objects
#' 
#' Prints out the \code{locus} to stdout.
#' @param x The \code{locus} object
#' @param \dots Other arguments passed to \code{\link{print.default}}.
#' @return Nothing
#' @method print locus
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' 
#' loc <- locus( 1:2 )
#' print( loc )
#' 
print.locus <- function(x, ... ) {
  x0 <- unlist( lapply( x, as.character ) )
  x0[x0=="NA"] <- NA
  x0[nchar(x0)==0] <- NA
  print.default(x0,...)
  invisible(x0)
}







#' Provides a summary of the \code{locus} object
#' 
#' Provides an overload of the \code{summary} object so that when you
#'  put this into a \code{data.frame} and ask for a summary, it will 
#'  provide you a summary of genotype counts.
#' @param object The \code{locus} object to summarize (typically a vector).
#' @param \dots Ignored
#' @return A summary of the vector of \code{locus} objects in \code{object}
#' @method summary locus
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>

summary.locus <- function(object,...) { 
  genos <- paste(as.character( object ), " ", sep="")
  genos[ genos==" " ] <- NA
  return( summary(factor(genos),maxsum=7))
}





#' An 'is-a' operator for \code{locus}
#' 
#' A quick convienence function to determine if an object is
#'  inherited from the \code{locus} object.
#' @param x An object to query
#' @return A logical flag indicating if \code{x} is a type of \code{locus}
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' 
#' loc1 <- locus( c("A","A") )
#' is.locus( loc1 )
#' is.locus( FALSE )
#' is.locus( 23 )
#' 
is.locus <- function ( x ) { 
  return( inherits(x,"locus"))
}





#' Overload '[' for vectors of \code{locus} objects
#' 
#' An overload of the \code{[} function for \code{locus} objects.
#' @param x An object of type \code{locus}
#' @param i The index of the allele to grab.
#' @return The allele at the ith position.
#' @rdname locus-operator-index
#' @method "[" locus
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' 
#' loci <- c( locus(1:2), locus(1:2), locus( c(1,1) ) )
#' loci[2]
#'
`[.locus` <- function (x, i) {
  y <- unclass(x)[i]
  class(y) <- "locus"
  y
}





#' Overload '+' operator for pairs of \code{locus} objects
#' 
#' An overload of the \code{+} operator for \code{locus} objects that
#'  results in the creation of an offspring \code{locus}.
#' @param e1 A \code{locus} object.
#' @param e2 A \code{locus} object.
#' @return A new \code{locus} object that represents an offspring 
#'  genotype.
#' @rdname locus-operator-plus
#' @method "+" locus
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' 
#' dad <- locus( c("A", "A") ) 
#' mom <- locus( c("B", "B") )
#' off <- mom + dad 
#' off
#'
`+.locus` <- function( e1, e2 ){
  if( is.na(e1) || is.na(e2))
    stop("Cannot add missing locus objects.")
  
  a1 = alleles(e1)
  a2 = alleles(e2)
  if( length(a1)!=length(a2))
    stop("Cannot add genotypes with different ploidy levels.")
  if( length(a1)<2 || length(a2)<2 )
    stop("Cannot add genotypes less than diploid")
  if( length(a1) %% 2 )
    stop("General addition is not supported for odd ploidy (too many assumptions), 
         you can write your own routine.")
  n <- length(a1)/2
  off <- locus(  c( sample(a1, size=n, replace=FALSE),
                   sample(a2, size=n, replace=FALSE)))
  return(off)
}




#' Overload '-' operator for pairs of \code{locus} objects
#' 
#' An overload of the \code{-} operator for \code{locus} objects that
#'  removes the contribution of a parental genotype to a offspring 
#'  genotype (if possible).
#' @param e1 A \code{locus} object reprenting the offspring.
#' @param e2 A \code{locus} object representing the parent.
#' @return A new \code{locus} object that represents the genotypes
#'  left over after removing the parental part (if possible).
#' @note In some cases it is not possible to remove parental alleles (e.g., 
#'  consider cases where both parent and offspring are the same 
#'  heterozygote).
#' @rdname locus-operator-minus
#' @method "-" locus
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' 
#' mom <- locus( c("A", "A") ) 
#' off <- locus( c("A", "B") )
#' dad.gamete <- off - mom
#' dad.gamete
#'
`-.locus` <- function( e1, e2 ){
  if( is.na(e1) || is.na(e2))
    stop("Cannot subtract missing locus objects.")
  
  off = alleles(e1)
  mom = alleles(e2)
  if( length(off)!=length(mom))
    stop("Cannot subtract genotypes with different ploidy levels.")
  if( length(off)<2 || length(mom)<2 )
    stop("Cannot subtract genotypes less than diploid")
  if( length(off) %% 2 )
    stop("General subtraction is not supported for odd ploidy (too many assumptions), 
         you can write your own routine.")
  
  int <- intersect(off,mom)
  
  # mother alleles not in offspring or not having half of the alleles
  if( length(int) < length(mom)/2 ) {
    warning(paste("Cannot subtract mom '",e2,"' from offspring '",e1,
                  "', result is unreduced.",sep=""))
    return(e1)
  }
  
  # mother and offspring both same
  else if( e1 == e2 ){
    
    # cant reduce heterozygotes
    if( is.heterozygote(e1))
      return(e1)
    
    # homozygote, return half of the offspring alleles
    else
      return( locus(off[1:(length(off)/2)]))
  }
  
  # mother and offspring different
  else {
    return(e1) #stop("Mother Offspring TODO")
  } 
}



