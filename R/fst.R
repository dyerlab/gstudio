#' Estimation Wright's Fst parameter
#' 
#' This function estimates Wright's Fst parameter, the one based upon the variance
#'  in p, most applicable to a 2-allele locus.  The results are returned
#'  as a \code{data.frame}.
#' @param x  A \code{data.frame} with \code{locus} objects.  If there is more than one 
#'  locus present, it will estimate the parameter for each locus.  It DOES NOT estimate
#'  the multilocus Fst (and do not even think of averaging the single locus estimates).
#' @param stratum Either the name of the column representing strata in \code{x}.  By 
#'  default, this function will use "Population".
#' @param nperm The number of permutations to run to test Fst=0 hypothesis.
#' @return An \code{data.frame} with Fst, sigma_p (variance among populations), and pq
#'  the total variance at the locus. 
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#'  AA <- locus( c("A","A") )
#'  AB <- locus( c("A","B") )
#'  BB <- locus( c("B","B") )
#'  locus <- c(AA,AA,AA,AA,BB,BB,BB,AB,AB,AA)
#'  Population <- c(rep("Pop-A",5),rep("Pop-B",5))
#'  df <- data.frame(Population,locus)
#'  Fst( df )
#'  locus2 <- c(AA,AA,AA,AA,AA,BB,BB,BB,BB,BB)
#'  df <- data.frame( Population, TPI=locus, PGM=locus2 )
#'  Fst( df )
Fst <- function( x, stratum="Population", nperm=0  ) {
  
  if( !is(x,"data.frame"))
    stop("This function requires you to pass it a data.frame of data...")
  
  x <- droplevels(x)
  
  if(nperm>0)
    warning("sorry, I just broke permutation for fst, check back when fixed.")
  
  loci <- column_class(x,"locus")
  if( any(is.na(loci) ) || ( !( any( loci %in% names(x)))))
    stop("You need to pass this function a data.frame with some columns of type 'locus' to it...")
    
  if( !(stratum %in% names(x)))
    stop("You need to pass this function the name of the column to use as a locus")
  
  hs <- He( x, stratum=stratum )
  ht <- Ht( x, stratum=stratum )
  ret <- merge( hs, ht)
  names(ret)[2] <- "Hs"
  ret$Fst <- 1 - ret$Hs/ret$Ht
  
  if( nperm > 0 ) {
    ret$P <- NA
  }
  
  return( ret )
}