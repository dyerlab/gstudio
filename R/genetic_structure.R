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
#'  \item{PhiST}{Phi_ST from an analysis of molecular variance (AMOVA; Excoffier
#'   et al. 1992) of the squared AMOVA distances among individuals (Smouse &
#'   Peakall 1999), the same distances as \code{genetic_distance(mode = "AMOVA")}}
#'  \item{PhiFT}{TwoGener Phi_FT (Smouse et al. 2001): differentiation of the
#'   pollen pools sampled by different mothers.  \code{x} holds mothers and their
#'   offspring; the maternal contribution is removed from each offspring with
#'   \code{\link{minus_mom}} and the paternal gametes are partitioned among maternal
#'   families as for PhiST.  The stratum is the maternal family (default \code{"ID"}
#'   for this mode) and mothers have \code{offspring} equal to "0"; see
#'   \code{data(cornus_florida)} for the layout.  An offspring with its mother's
#'   heterozygous genotype (an ambiguous paternal allele) is represented by its
#'   expected allele vector; a gamete incompatible with its mother is set to
#'   missing at that locus.  A mother not genotyped at a locus has her genotype
#'   reconstructed from her offspring by maximum likelihood, used only when it is
#'   at least 20 times as likely as the next best genotype; otherwise her offspring
#'   are set to missing at that locus.  Each reconstruction is reported in a
#'   message and in the \code{"maternal_inference"} attribute of the result.}
#' }
#' @param nperm The number of permutations used to test the hypothesis that
#'  the parameter = 0 (default=0).
#' @param size.correct A flag indicating that the estimate should be corrected
#'  based upon sample sizes (default=TRUE).
#' @param offspring For \code{mode = "PhiFT"} only: the column of offspring numbers,
#'  "0" for mothers (default "OffID").
#' @param pairwise A flag indicating that the analysis should be done among all pairs of 
#'  strata (default=FALSE).
#' @param locus An optional parameter specifying the locus or loci to be used
#'  in the analysis. If this is not specified, then all loci are used.
#' @return An object of type \code{data.frame} with columns \code{Locus}, the
#'  estimate (named for the mode: \code{Gst}, \code{Gst_prime}, \code{Dest} or
#'  \code{Fst}), \code{Hs}, \code{Ht} and, when \code{nperm > 0}, \code{p.value},
#'  in that order for every mode.  There is one row per locus and, when more than one locus is analysed, a
#'  \code{Multilocus} row whose \code{Hs} and \code{Ht} are means across loci.
#'  The multilocus estimate is 1 - mean(Hs)/mean(Ht) for \code{Gst} and \code{Fst}
#'  (Culley et al. 2001), and the harmonic mean of the per-locus estimates for
#'  \code{Gst_prime} and \code{Dest}.  Monomorphic loci have no estimate (\code{NA})
#'  and are left out of the multilocus estimate, as are negative per-locus
#'  \code{Gst_prime} and \code{Dest} values.  With \code{nperm > 0},
#'  \code{p.value} holds add-one permutation p-values; strata are permuted across
#'  individuals (the same permutation for every locus) so the \code{Multilocus} row
#'  is tested too.
#'
#'  \code{PhiST} and \code{PhiFT} have their own layout: \code{Locus}, the
#'  estimate, \code{sigma2_among}, \code{sigma2_within} and (when \code{nperm > 0})
#'  \code{p.value}, where
#'  Phi = sigma2_among / (sigma2_among + sigma2_within).  \code{PhiFT} also has
#'  \code{Nep} = 1 / (2 PhiFT), the effective number of pollen donors (\code{NA}
#'  when PhiFT <= 0), before \code{p.value}.  Individuals with a missing stratum
#'  are dropped, each locus uses the individuals genotyped at it, and the
#'  \code{Multilocus} row sums the variance components across loci (with complete
#'  data, the same as an AMOVA of the distances summed across loci).  As usual for
#'  AMOVA, sigma2_among and so Phi can be negative when there is no structure;
#'  negative values are kept, including in the \code{Multilocus} sums.
#'  \code{size.correct} does not apply to these two modes.
#'
#'  If \code{pairwise=TRUE}, it returns a symmetric matrix of the multilocus
#'  estimate (or the single-locus estimate when only one locus is used) between
#'  every pair of strata.  For \code{PhiST} and \code{PhiFT} the distances (and,
#'  for \code{PhiFT}, the gametes and allele frequencies) come from the whole
#'  sample, so each pair is a sub-analysis of the global one.
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
#'  genetic_structure( df, mode="PhiST", nperm=99)
#'  data(cornus_florida)
#'  genetic_structure( cornus_florida, mode="PhiFT" )
#'  genetic_structure( df, mode="Gst_prime", pairwise=TRUE)
genetic_structure <- function( x, stratum="Population", mode=c("Gst", "Gst_prime", "Dest", "Fst", "PhiST", "PhiFT")[1], nperm=0, size.correct=TRUE, pairwise=FALSE, locus, offspring="OffID" ) {
  
  if( !inherits(x,"data.frame") )
      stop("You need to pass a data frame to the function genetic_structure()...")
  
  is_ft <- tolower(mode[1]) %in% c("phift", "phi_ft")
  explicit <- !missing(stratum)
  if( is_ft && !explicit )
    stratum <- "ID"
  stratum <- .detect_stratum(x, stratum, default = if( is_ft ) "ID" else "Population", explicit = explicit)
  x <- .plain_df(x)
    
  if( !(stratum %in% names(x) ) ) 
    stop("You must specify which stratum to use for the estimation of genetic structure.")

  mode_clean <- tolower(mode)
  if( !(mode_clean %in% c("fst", "gst", "gst_prime", "dest", "gst'", "phist", "phi_st", "phift", "phi_ft")) )
    stop(paste("The structure mode", mode, "is not recognized"))
  
  # subsets of loci
  if( !missing( locus ) ){
    
    if( all( locus %in% column_class(x,"locus")) ) 
      x <- x[, c(stratum, if( is_ft ) offspring, locus) ]
    else
      stop(paste("At least one of the loci you requested", paste(locus,collapse=", "), "is not in the data.frame you passed"))
  }
  
  is_st <- mode_clean %in% c( "phist", "phi_st" )
  if( pairwise && is_st )
    return( PhiST( x, stratum = stratum, pairwise = TRUE ) )
  if( pairwise && is_ft )
    return( PhiFT( x, stratum = stratum, offspring = offspring, pairwise = TRUE ) )

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
          r <- genetic_structure( y, stratum, mode_clean, nperm=0, size.correct, offspring=offspring )
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

    else if( mode_clean %in% c( "phist", "phi_st" ) )
      ret <- PhiST( x, stratum = stratum, nperm = nperm )

    else if( mode_clean %in% c( "phift", "phi_ft" ) )
      ret <- PhiFT( x, stratum = stratum, offspring = offspring, nperm = nperm )

    names(ret)[names(ret) == "P"] <- "p.value"

    # p-values come only from permutation: without it there is no p.value column
    if( nperm > 0 ) {
      if( !("p.value" %in% names(ret)) )
        ret$p.value <- NA_real_
    } else
      ret$p.value <- NULL
  }
  

  
  return( ret )
}



