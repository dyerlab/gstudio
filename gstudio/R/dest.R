#' Estimation Joost's Dest parameter 
#' 
#' This function estimates the parameter (and potentially the confidence
#'  surrounding its value) for Joost's Dest.
#' @param strata A partitioning of data into groups
#' @param loci A list of \code{\link{locus}} objects partitioned by stratum.
#' @param nperm The number of permutations to run for significance of the
#'  estimator.
#' @param size.correct A flag indicating that the estimate should be corrected for
#'  based upon sample sizes (default=TRUE).
#' @return An object of type "structure statistic"
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
Dest <- function( strata, loci, nperm=0, size.correct=FALSE ) {
  if( !inherits(loci,"locus") )
    stop("This function requires objects of type 'locus' to function.")
  if( !is(strata,"factor") )
    strata <- factor( strata)
  
  k <- length(levels(strata))
  strata.lvls <- levels(strata)
  
  totfreq <- frequencies( loci )
  inds <- to_mv.locus(loci,alleles=alleles(totfreq) )
  p.vec <- colSums(inds)
  ht <- 1-sum((p.vec/sum(p.vec))^2)
  hs <- mean(unlist(lapply( strata.lvls, 
                            function(strat,inds,strata ) {
                              s <- colSums(as.matrix(inds[strata==strat,]))
                              f <- 1-sum((s/sum(s))^2)
                            }, inds=inds, strata=strata)))
  
  
  n.harmonic <- 1/mean(1/table(strata))
  hs.estimated <- (2*n.harmonic)/(2*n.harmonic -1) * hs
  ht.estimated <- ht + hs.estimated/(2*k*n.harmonic)
  
  if( size.correct ) 
    D <- ((ht.estimated-hs.estimated) / (1-hs.estimated))  
  else
    D <- ((ht-hs) / (1-hs))
  
  D <- D / (k/(k-1))
  
  if( nperm > 0 ) {
    perms <- rep(NA,nperm)
    for(i in 1:nperm ) {
      hs.perm <- lapply( strata.lvls, 
                         function(strat,inds,strata ) {
                           s <- colSums(as.matrix(inds[strata==strat,]) )
                           f <- 1 - sum((s/sum(s))^2) 
                         }, 
                         inds=inds, 
                         strata=sample(strata))
      perms[i] <- mean(unlist(hs.perm))
    }
    if( size.correct ) {
      perms <- (2*n.harmonic) / (2*n.harmonic-1) * perms
      D.perm <- ((ht.estimated-perms) / (1-perms))      
    }
    else
      D.perm <- (ht-perms) /(1-perms)
    
    D.perm <- D.perm / (k/(k-1))
  }
  else
    D.perm <- numeric(0)
  
  ret <- structure_statistic( mode="Dest", estimate=D, ci=D.perm, Hs=hs.estimated, Ht=ht.estimated )
  
  return( ret )
}