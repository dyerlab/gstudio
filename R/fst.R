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
#'  the total variance at the locus.  When \code{nperm > 0} it also has \code{P},
#'  the right-tailed add-one permutation p-value for the null hypothesis Fst = 0
#'  (small P indicates significant structure).
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
  
  hs <- Hes( x, stratum=stratum )
  ht <- Ht( x, stratum=stratum )
  ret <- merge( hs, ht)
  names(ret)[2] <- "Hs"
  ret$Fst <- 1 - ret$Hs/ret$Ht
  
  if( nperm > 0 ) {
    ret$P <- 0
    tmp <- x
    message("permuting ", appendLF = FALSE)
    for( rep in seq(1,nperm) ) { 
      
      if( rep%%10 == 0 ) { 
        message(".", appendLF = FALSE)
      }
      # Label permutation (no replacement) for the H0: Fst = 0 null.
      tmp[[stratum]] <- sample( tmp[[stratum]] )
      suppressWarnings(
        Fst <- 1.0 - Hes( tmp, stratum=stratum, do.multilocus=FALSE )$Hes / ht$Ht
      )
      # Right tail: count permutations whose Fst meets/exceeds the observed.
      bigger <- ifelse( Fst >= ret$Fst, 1, 0 )
      ret$P <- ret$P + bigger
    }
    # Add-one permutation p-value: (1 + #{perm >= obs}) / (1 + nperm), strictly
    # positive (Phipson & Smyth 2010, Stat. Appl. Genet. Mol. Biol. 9:Article39).
    ret$P <- ( 1 + ret$P ) / ( 1 + nperm )
  }
  
  return( ret )
}