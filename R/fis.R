#' Estimate simple inbreeding from frequencies
#' 
#' Returns the general Fis = 1-ho/he parameter from the locus being passed or
#'  a set of them if the value passed is a \code{data.frame} with locus objects.
#'  
#' @param x Either a \code{locus} object or a data.frame with locus objects.
#' @param small.N Passes this along to He for small sample sizes.
#' @param stratum An optional term for the estimation of inbreeding from samples of populations 
#'  (default=NULL)
#' @param loci The subset of loci to use (if more are passed), default is to use
#'  all loci.
#' @return The inbreeding F statistic as a \code{numeric} value or a \code{data.frame}
#'  if you passed multiple loci to this function.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' loci <- c( locus( c("A","A") ), locus( c("A","A") ), locus( c("A","B") ) )
#' Fis( loci )
Fis <- function( x, small.N=FALSE, stratum=NULL, loci=NULL ) {
  
  if( is(x,"locus") ) {
    ret <- 1.0 - Ho(x) / He(x, small.N)
    names(ret) <- "Fis"
  }
  else if( is(x,"data.frame")  ) {
    x <- droplevels(x)
    
    if( is.null(loci) )
      loci <- column_class(x,"locus")
    
    ho <- genetic_diversity( x, mode="Ho", small.N=small.N, stratum=stratum )
    he <- genetic_diversity( x, mode="He", small.N=small.N, stratum=stratum )
    ho <- ho[ ho$Locus %in% loci,]
    he <- he[ he$Locus %in% loci,]
    
    if( is.null(stratum)) {
      ret <- data.frame( Locus=ho$Locus, Fis = 1.0 - ho$Ho / he$He )
      if( nrow( ret ) > 1 && is.null(stratum) )
        ret <- rbind( ret, data.frame(Locus="Multilocus", Fis= (1.0 - sum(ho$Ho)/sum(he$He))))
    }
    else {
      strata <- unique( x[[stratum]] )
      k <- length( strata )
      ret <- data.frame( Stratum=rep(strata, each=length(loci)), Locus=ho$Locus, Fis = 1.0 - ho$Ho / he$He )
    }
  }
  else
    stop(paste("This function does not know how to handle data of type",class(x)))
  
  return( ret )
}
