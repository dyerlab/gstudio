#' Estimation of , genetic structure statistics.
#' 
#' This function estimates common , genetic statistics.
#' @param x An object of type \code{data.frame} with at least a single column
#'  of type \code{\link{locus}}
#' @param stratum The stratum to use as groupings (default='Population').
#' @param mode Whic statistic to estimate.  Current options include:
#' \describe{
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
#' @param locus An optional parameter specifying the locus or loci to be used
#'  in the analysis.  If this is not specified, then all loci are used.
#' @return An object of type \code{data.frame} containing estimates for each locus and a
#'  multilocus estiamte.  If \code{pairwise=TRUE}, then it returns the multilocus (if more
#'  than one locus) estiamte in a matrix format.
#' @note The multilocus estimation of these parameters is estimated following the
#'  suggestions of Culley et al. (2001) A comparison of two methods of calculating Gst, 
#'  a genetic measure of population differentiation.  American Journal of Botany 89(3): 460-465.
#' @export
#' @examples
#'  AA <- locus( c("A","A") )
#'  AB <- locus( c("A","B") )
#'  BB <- locus( c("B","B") )
#'  locus <- c(AA,AA,AA,AA,BB,BB,BB,AB,AB,AA)
#'  locus2 <- c(AB,BB,AA,BB,BB,AB,AB,AA,AA,BB)
#'  Population <- c(rep("Pop-A",5),rep("Pop-B",5))
#'  df <- data.frame( Population, TPI=locus, PGM=locus2 )
#'  genetic_structure( df, mode="Gst", nperm=999)
#'  genetic_structure( df, mode="Gst", pairwise=TRUE)
#'  genetic_structure( df, mode="Gst", pairwise=TRUE, locus="TPI" )
genetic_structure <- function( x, stratum="Population", mode=c("Gst", "Gst_prime", "Dest")[1], nperm=0, size.correct=TRUE, pairwise=FALSE, locus ) {
  
  if( !inherits(x,"data.frame") )
      stop("You need to pass a data frame to the funciton genetic_structure()...")
    
  if( !(stratum %in% names(x) ) ) 
    stop("You must specify which stratum to use for the estimation of genetic structure.")

  if( !(mode %in% c("Gst", "Gst_prime", "Dest")) )
    stop(paste("The structure mode",mode,"is not recognized") )
  
  # subsets of loci
  if( !missing( locus ) ){
    
    if( all( locus %in% column_class(x,"locus")) ) 
      x <- x[, c(stratum,locus) ]
    else
      stop(paste("At least one of the loci you requested", paste(locus,collapse=", "), "is not in the data.frame you passed"))
  }
  
  # do this in a pair-wise fashion
  if( pairwise ) {
    pops <- levels( factor( x[[stratum]] ))
    K <- length(pops)
    ret <- matrix(0,nrow=K,ncol=K)
    rownames(ret) <- colnames(ret) <- pops
    diag(ret) <- NA
    for( i in 1:K){
      for( j in i:K){
        if( i!=j ){
          y <- rbind(x[x[[stratum]]==pops[i],], x[x[[stratum]]==pops[j],])
          r <- genetic_structure( y, stratum, mode, nperm=0, size.correct )
          ret[i,j] <- ret[j,i] <-  r[nrow(r),2]
        } 
      }
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



