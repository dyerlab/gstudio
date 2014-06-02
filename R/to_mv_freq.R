#' Returns mv frequencies for stratum in long format
#' 
#' This function takes a \code{data.frame} of data and returns
#'  a matrix of allele frequencies where each row is a stratum
#'  and each column is an allele frequency (all lumped together).
#' @param x A \code{data.frame} object with \code{locus} columns
#' @param stratum The stratum to use to partition the data (default
#'  "Population")
#' @return A matrix with as many rows as strata and columns as 
#'  alleles across all loci.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#'   AA <- locus( c("A","A") )
#'   AB <- locus( c("A","B") )
#'   AC <- locus( c("A","C") )
#'   BB <- locus( c("B","B") )
#'   BC <- locus( c("B","C") )
#'   CC <- locus( c("C","C") )
#'   loci <- c(AA,AA,AB,AA,BB,BC,CC,BB,BB,CC)
#'   df <- data.frame( Population=c(rep("A",5),rep("B",5) ), TPI=loci )
#'   to_mv_freq(df)
to_mv_freq <- function( x, stratum="Population") {
  if(!is(x,"data.frame"))
    stop("This function only works with a data.frame object")
  if( !( stratum %in% names(x)))
    stop("The stratum name you provided is not in the data.frame")
  
  x[[stratum]] <- factor( as.character(x[[stratum]]))
  
  s <- x[[stratum]]
  if( !is(s,"factor"))
    s <- factor(s)
  strata_names <- levels(s)
  K <- length(strata_names)
  m <- to_mv( x )
  ret <- matrix(0,nrow = K, ncol=ncol(m))
  rownames(ret) <- strata_names
  
  for( i in 1:K ){
    d <- m[ s==strata_names[i],]
    if( is(d,"numeric") ) 
      ret[i,] <- d
    else
      ret[i,] <- colSums(d)/nrow(d)

  }
  return( ret )
}