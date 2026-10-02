#' Estimation of genetic structure and differentiation statistics
#' 
#' This function is the primary front-end for estimating common genetic structure
#'  and differentiation statistics across loci and strata.
#' @param x An object of type \code{data.frame} with at least a single column
#'  of type \code{\link{locus}}.
#' @param stratum The stratum to use as groupings (default='Population').
#' @param mode Which statistic to estimate. Current options include:
#' \describe{
#'  \item{Fst}{Wright's Fst parameter based upon variance in allele frequencies}
#'  \item{Gst}{Nei's Gst parameter}
#'  \item{Gst_prime}{Hedrick's (2005) standardized Gst' for diverse loci}
#'  \item{Dest}{Jost's (2008) D_est differentiation estimate}
#' }
#' @param nperm The number of permutations used to test the hypothesis that
#'  the parameter = 0 (default=0).
#' @param size.correct A flag indicating that the estimate should be corrected
#'  based upon sample sizes (default=TRUE).
#' @param pairwise A flag indicating that the analysis should be done among all pairs of 
#'  strata (default=FALSE).
#' @param locus An optional parameter specifying the locus or loci to be used
#'  in the analysis. If this is not specified, then all loci are used.
#' @return An object of type \code{data.frame} containing estimates for each locus and a
#'  multilocus estimate. If \code{pairwise=TRUE}, then it returns a pairwise matrix.
#' @note The multilocus estimation of Gst parameters is estimated following the
#'  suggestions of Culley et al. (2001) American Journal of Botany 89(3): 460-465.
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
#'  genetic_structure( df, mode="Fst")
#'  genetic_structure( df, mode="Dest")
#'  genetic_structure( df, mode="Gst_prime", pairwise=TRUE)
genetic_structure <- function( x, stratum="Population", mode=c("Gst", "Gst_prime", "Dest", "Fst")[1], nperm=0, size.correct=TRUE, pairwise=FALSE, locus ) {
  
  if( !inherits(x,"data.frame") )
      stop("You need to pass a data frame to the function genetic_structure()...")
  
  stratum <- .detect_stratum(x, stratum, default = "Population")
    
  if( !(stratum %in% names(x) ) ) 
    stop("You must specify which stratum to use for the estimation of genetic structure.")

  mode_clean <- tolower(mode)
  if( !(mode_clean %in% c("fst", "gst", "gst_prime", "dest", "gst'")) )
    stop(paste("The structure mode", mode, "is not recognized"))
  
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
          r <- genetic_structure( y, stratum, mode_clean, nperm=0, size.correct )
          ret[i,j] <- ret[j,i] <-  r[nrow(r),2]
        } 
      }
    }
    
    return( ret )
  }
  
  else {
    ret <- data.frame()
    
    if( mode_clean == "gst") 
      ret <- Gst( x, stratum=stratum, nperm=nperm, size.correct=size.correct )
    
    else if( mode_clean %in% c("gst_prime", "gst'") ) 
      ret <- Gst_prime( x, stratum=stratum, nperm=nperm, size.correct=size.correct )
    
    else if( mode_clean == "dest" ) 
      ret <- Dest( x, stratum=stratum, nperm=nperm, size.correct=size.correct )
    
    else if( mode_clean == "fst" ) 
      ret <- Fst( x, stratum, nperm )
  }
  

  
  return( ret )
}



