#' Estimation of , genetic structure statistics.
#' 
#' This function estimates common , genetic statistics.
#' @param x An object of type \code{data.frame} with at least a single column
#'  of type \code{\link{locus}}
#' @param stratum The stratum to use as groupings (default='Population').
#' @param mode Whic statistic to estimate.  Current options include:
#' \itemize{
#'  \item{Gst}{Nei's Gst (not Berg and Hamrick)}
#'  \item{Gst_prime}{Hedrick's correction of Nei's Gst for diverse loci}
#'  \item{Dest}{Joost's estimate}
#' }
#' @param nperm The number of permutations used to test the hypothesis that
#'  the parameter = 0.
#' @param size.correct A flag indicating that the estimate should be corrected for
#'  based upon sample sizes (default=TRUE).
#' @param pairwise A flag indicating that the analysis should be done among all pairs of 
#'  strata.
#' @return An object of type \code{genetic_structure}.
#' @note The multilocus estimation of these parameters is estimated following the
#'  suggestions of Culley et al. (2001) A comparison of two methods of calculating Gst, 
#'  a genetic measure of , differentiation.  American Journal of Botany 89(3): 460-465.
#' @export
#' @examples
#'  AA <- locus( c("A","A") )
#'  AB <- locus( c("A","B") )
#'  BB <- locus( c("B","B") )
#'  locus <- c(AA,AA,AA,AA,BB,BB,BB,AB,AB,AA)
#'  locus2 <- c(AB,BB,AA,BB,BB,AB,AB,AA,AA,BB)
#'  Population <- c(rep("Pop-A",5),rep("Pop-B",5))
#'  df <- data.frame( Population, TPI=locus, PGM=locus2 )
#'  genetic_structure( df, mode="Gst")
genetic_structure <- function( x, stratum="Population", mode=c("Gst", "Gst_prime", "Dest")[1], nperm=0, size.correct=TRUE, pairwise=FALSE ) {
  
  if( !inherits(x,"data.frame") )
      stop("You need to pass a data frame to the funciton genetic_structure()...")
    
  if( !(stratum %in% names(x) ) ) 
    stop("You must specify which stratum to use for the estimation of genetic structure.")

  if( !(mode %in% c("Gst", "Gst_prime", "Dest")) )
    stop(paste("The structure mode",mode,"is not recognized") )
  
  # do this in a pair-wise fashion
  if( pairwise ) {
    loci <- column_class(x,"locus")
    pops <- levels( factor( x[[stratum]] ))
    K <- length(pops)
    ret <- list()

    for( locus in loci ){
      m <- matrix(0,nrow=K,ncol=K)
      for( i in 1:K){
        for( j in i:K){
          if( i!=j ){
            pop <- rbind( x[x[[stratum]] == pops[i], c(stratum,locus)], x[x[[stratum]] == pops[j],c(stratum,locus)] )
            r <- genetic_structure( pop, stratum, mode, nperm=0, size.correct )
            m[i,j] <- m[j,i] <- r[1,2]
          } 
        }
      }
      ret[[locus]] <- m
    }
    return( ret )
  }
  
  
  else {
    
    mode <- tolower( mode )
    
    ret <- data.frame()
    
    if( mode == "gst") 
      ret <- Gst( x, stratum, nperm, size.correct )
    
    else if( mode == "gst_prime") 
      ret <- Gst_prime( x, stratum, nperm, size.correct )
    
    else if( mode == "dest" ) 
      ret <- Dest( x, stratum, nperm, size.correct )
    
  }
  

  
  return( ret )
}



print.genetic_structure <- function( x, ... ){
  
  cat("Genetic Structure\n")
  print(as.data.frame.genetic_structure(x))
  
}


