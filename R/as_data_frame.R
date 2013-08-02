


#' Conversion of genetic_structure to data.frame
#' 
#' Converts the \code{\link{genetic_structure}} object to a common
#'  data.frame object with keys 'locus', 'Allele' and 'Frequency'
#' @param x An objet of type \code{\link{genetic_structure}}
#' @param ... Ignored
#' @return A data frame
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @method as.data.frame genetic_structure
as.data.frame.genetic_structure <- function(x, ...) {
  df <- data.frame( unlist(lapply( x$loci, function(x) return( x$estimate )) ) , stringsAsFactors=FALSE)
  names(df)[1] <- x$mode
  df$Hs <- unlist( lapply( x$loci, function(x) return( x$Hs ) ) )
  df$Ht <- unlist( lapply( x$loci, function(x) return( x$Ht ) ) )
  
  muHs <- mean(df$Hs)
  muHt <- mean(df$Ht)
  muParam <- 1 - muHs/muHt
  
  df <- rbind( df, c(muParam, muHs, muHt))
  rownames(df)[dim(df)[1]] <- "Multilocus"
  
  return(df)
  
}

#' Converts locus to a data frame
#' 
#' This converts a \code{locus} object into a \code{data.frame} so that
#'  you can use \code{locus} objects in a column of a \code{data.frame}.
#' @param x An object of type \code{code}.  This can be either a single genotype 
#'  (a rare case) or a vector of genotypes (preferred). 
#' @param \dots Additional objects that are passed to \code{as.data.frame.vector}.
#' @return A \code{data.frame} object.
#' @note If you do not assign a data name to the \code{x} in assignment (e.g., 
#'  TPI=x ) it will name the column in the \code{data.frame} the same as the name
#'  of the variable you assigned it.  If this is confusing see the examples.
#' @method as.data.frame locus
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' 
#' loc1 <- c( locus(1:2), locus(1:2), locus( c(1,1) ) )
#' df <- data.frame( ID=1:3, NAMED_LOCUS=loc1, loc1 )
#' summary(df)
#' 
as.data.frame.locus <- function( x, ... ) {
  nm <- deparse( substitute(x), width.cutoff = 500L )
  return( as.data.frame.vector(x, ..., nm=nm ) ) 
}










