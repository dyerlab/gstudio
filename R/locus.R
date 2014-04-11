#' General constructor for locus object
#' 
#' This function creates an object of type \code{locus}, a fundamental
#'  type in the gstudio package.  Therea re several kinds of loci that
#'  can be created.
#' @param x The data to be turned into a \code{locus} object
#' @param type An indication of what kind of data it is.  By default this
#'  parameter is missing and this will cause the function to assume that 
#'  every element of x is an allele in the genotype.
#'  \describe{
#'    \item{blank}{Default value, uses all passed items as alleles}
#'    \item{aflp}{Encoded as 0,1 for absence/presence of bands.}
#'    \item{column}{Two columns of alleles}
#'    \item{separated}{Pre-separated alleles (with ':').}
#'    \item{snp}{Encoded by the number of minor alleles at the locus.}
#'    \item{zyme}{Alleles like zymes (e.g., 12 for '1' and '2' alleles).}
#' }
#' @param phased A flag indicating the the alleles should are of
#'  known gametic phase (default=FALSE).
#' @return Either a single or vector of objects of type \code{locus}.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#' AA <- locus( c("A","A") )
#' AB <- locus( c("A","B") )
#' BB <- locus( c("B","B") )
#' AC <- locus( c("A","C") )
#' AD <- locus( c("A","D") )
#' BC <- locus( c("B","C") )
#' BD <- locus( c("B","D") )
#' CC <- locus( c("C","C") )
#' CD <- locus( c("C","D") )
#' DD <- locus( c("D","D") )
#' loci <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD) 
#' loci
locus <- function( x, type, phased=FALSE ){
  
  # missing data
  if( missing(x) || (length(x)==1 & nchar(x)==0) )
    ret <- ""
  
  # default, sort and collapse em.
  else if( missing(type) ){  
    ret <- as.character(x)
    if( any(nchar(ret))) {
      if( !phased )
        ret <- as.character(sort(x))
      ret <- paste(ret,collapse=":")
      if( ret == "NA:NA")
        ret <- ""
    }
  }
  
  # aflp
  else if( type == "aflp" ){
    if( length(x) > 1 ) 
      ret <- unlist(lapply(x,function(x) locus(as.character(x),type="aflp"))) 
    else if( !(x %in% c("0","1")) )
      ret <- ""
    else
      ret <- x
  }
  
  else if( type == "snp"){
    if( length(x) > 1 )
      ret <- unlist(lapply(x,function(x) locus(as.character(x),type="snp")))
    else 
      ret <- switch( as.character(x),"0"=c("A:A"),"1"=c("A:B"),"2"=c("B:B"),"")
  }
  
  #column types
  else if( type == "column") 
    ret <- apply( x, 1, function(x) locus(as.character(x), phased=phased))
  
  
  else if( type == "separated" ) {
    if( length(x) > 1)
      ret <- unlist(lapply(x,function(x) locus(x)))
    else {
      if( x == "NA:NA" || x == "NA")
        ret <- ""
      else 
        ret <- locus( strsplit( x, split=":")[[1]], phased=phased)
    }
      
  } 
  
  else if( type == "zyme" ){
    
    if( length(x) > 1 )
      ret <- unlist( lapply(x, function(x) locus(x,type="zyme")))
    else {
      N <- nchar(x)
      n <- N/2
      l <- substr(x,1,n)
      r <- substr(x,(n+1),N)
      ret <- locus(c(l,r))
    }
  }
  
  
  class(ret) <- "locus"
  return(ret)
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
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
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
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
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
  if( inherits(x,"list"))
    x <- unlist(x)
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
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
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
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
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
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}

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
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
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


#' Replicate a locus
#' 
#' A quick replacement for \code{rep} so it does not 
#'  replicate a \code{locus} object as a character so
#'  we can use functions like \code{outer} efficiently.
#' @param x An object of type character
#' @param times The number of times to replicate this
#' @param ... Ignored
#' @return A vector of \code{locus} objects
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' 
#' loc1 <- locus( c("A","B"))
#' rep(loc1, times=4)
rep.locus <- function( x, times,... ){
  c <- as.character(x)
  ret <- rep(c,times=times)
  class(ret) <- "locus"
  return(ret)
}






#' Overload '[' for vectors of \code{locus} objects
#' 
#' An overload of the \code{[} function for \code{locus} objects.
#' @param x An object of type \code{locus}
#' @param i The index of the allele to grab.
#' @return The allele at the ith position.
#' @rdname locus-operator-index
#' @method [ locus
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' 
#' loci <- c( locus(1:2), locus(1:2), locus( c(1,1) ) )
#' loci[2]
#'
`[.locus` <- function (x, i) {
    y <- unclass(x)[i]
    class(y) <- "locus"
    return(y)  
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
#' @method + locus
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
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
#' @method - locus
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' 
#' mom <- locus( c("A", "A") ) 
#' off <- locus( c("A", "B") )
#' dad.gamete <- off - mom
#' dad.gamete
#'
`-.locus` <- function( e1, e2 ){
  
  
  if( length(e1) > 1 ){
    ret <- locus()
    for( i in 1:length(e1)){
      ret <- c( ret, e1[i] - e2[i] )
    }
    return( ret[-1] )
  }
  
  
  if( is.na(e1) || is.na(e2)) {
    warning("Subtract missing locus objects.  Nothing subtracted.")
    return( e1 )
  }
    
  
  off <- alleles(e1)
  mom <- alleles(e2)
  if( length(off)!=length(mom))
    stop("Cannot subtract genotypes with different ploidy levels.")
  if( length(off)<2 || length(mom)<2 )
    stop("Cannot subtract genotypes less than diploid")
  if( length(off) %% 2 )
    stop("General subtraction is not supported for odd ploidy (too many assumptions), 
         you can write your own routine.")
  
  # mother and offspring both same
  if( e1 == e2 ){
    
    # cant reduce heterozygotes
    if( is_heterozygote(e1))
      return(e1)
    
    # homozygote, return half of the offspring alleles
    else
      return( locus(off[1:(length(off)/2)]))
  }
  
  else { 
    
    int <- intersect(off,mom)
    
    # mother alleles not in offspring or not having half of the alleles
    if( length(int) == 0 ) {
      message(paste("Unable to subtract adult '",e2,"' from offspring '",e1,
                    "', result is unreduced.",sep=""))
      return(e1)
    }  
    
    # mother and offspring different
    else {
      mom_allele <- which( off==int)[1]
      offidx <- 1:length(off)
      
      ret <- locus( off[ offidx[ offidx != mom_allele]])
      return(ret) 
    }
  }
  
   
}








