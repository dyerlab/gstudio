#' Estimate simple inbreeding from frequencies
#' 
#' Returns the general Fis = 1-ho/he parameter from the locus being passed or
#'  a set of them if the value passed is a \code{data.frame} with locus objects.
#'  
#' @param x Either a \code{locus} object or a data.frame with locus objects.
#' @param small.N Passes this along to He for small sample sizes.
#' @param stratum An optional term for the estimation of inbreeding from samples of populations 
#'  (default=NULL)
#' @return The inbreeding F statistic as a \code{numeric} value or a \code{data.frame}
#'  if you passed multiple loci to this function.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' loci <- c( locus( c("A","A") ), locus( c("A","A") ), locus( c("A","B") ) )
#' Fis( loci )
Fis <- function( x, small.N=FALSE, stratum=NULL ) {
  
  if( is(x,"locus") ) {
    ret <- 1.0 - Ho(x) / He(x, small.N)
    names(ret) <- "Fis"
  }
  else if( is(x,"data.frame")  ) {
    if( is.null(stratum)){
      ho <- Ho( x, stratum=stratum )
      he <- He( x, stratum=stratum )
      loci <- column_class(x,"locus")
      ho <- ho[ ho$Locus %in% loci,]
      he <- he[ he$Locus %in% loci,]
      Fis <- 1.0 - ho$Ho/he$He
      ret <- data.frame( Locus=loci, Fis )
    }
    else {
      if( !(stratum %in% names(x)) )
        stop("Cannot find this stratum in the data.frame...")
      
      pops <- partition(x,stratum)
      ret <- NULL
      for( pop in names(pops)){
        ret1 <- Fis(pops[[pop]],small.N=small.N)
        ret1[["Stratum"]] <- pop
        ret <- rbind( ret, ret1 )
      }
      ret <- ret[,c(3,1,2)]
      
    }
  }
  else
    stop(paste("This function does not know how to handle data of type",class(x)))
  
  return( ret )
}
