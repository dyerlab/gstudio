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
Fst <- function( x, stratum="Population", loci=NULL ) {
  if( !is(x,"data.frame"))
    stop("This function requires you to pass it a data.frame of data...")
  
  if( is.null(loci))
    loci <- column_class(x,"locus")
  if( any(is.na(loci) ) || ( !( any( loci %in% names(x)))))
    stop("You need to pass this function a data.frame with some columns of type 'locus' to it...")
    
  if( !(stratum %in% names(x)))
    stop("You need to pass this function the name of the column to use as a locus")
  
  tot_freqs <- frequencies( x, loci=loci )
  pop_freqs <- frequencies( x, stratum, loci = loci)
  numpops <- length(unique(x[[stratum]]))
  ret <- data.frame( Locus=loci, Fst=0, sigma2=0, pq=0 )
  for( locus in loci){
    f <- frequencies( x[[locus]] ) 
    if( nrow(f) > 1){
      a <- f[1,1]
      p <- f[1,2]
      phat <- freqs$Frequency[ freqs$Allele==a & freqs$Locus==locus]
      if( length(phat) < numpops )
        phat <- c(phat, rep(0,numpops-length(phat)) )
      pq <- p*(1-p)*length(phat)
      varp <- var(phat)
      
      ret$sigma2[ ret$Locus==locus ] <- varp
      ret$pq[ ret$Locus==locus ] <- pq
      ret$Fst[ ret$Locus==locus ] <- varp/pq
    }
  }
  
  return( ret )
}