#' Estimation Nei's Gst parameter
#' 
#' This function estimates Nei's Gst parameter and potentially
#'  returns the components of it as well as the probability.  The results are returned
#'  as a \code{data.frame}.
#' @param strata A partitioning of data into groups
#' @param loci  A list of \code{\link{locus}} objects partitioned by stratum.
#' @param nperm The number of permutations to run for significance of the
#'  estimator.
#' @param size.correct A flag indicating that the estimate should be corrected for
#'  based upon sample sizes (default=TRUE).
#' @return An \code{data.frame} with Gst, Ht, and Hs and optionally P
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#'  AA <- locus( c("A","A") )
#'  AB <- locus( c("A","B") )
#'  BB <- locus( c("B","B") )
#'  locus <- c(AA,AB,AA,AA,BB,BB,BB,AB,AB,AA)
#'  Population=c(rep("Pop-A",5),rep("Pop-B",5))
#'  Gst( locus, Population, nperm=99 )
Gst <- function(strata, loci, nperm=0, size.correct=TRUE ) {
  
  # Do this function recursively if a data.frame is passed as loci
  if( is(loci,"data.frame") ) {
    loci_df <- column_class(loci,"locus")
    
    if( length(loci_df)==0 )
      stop("You must pass some loci to this function")
    
    ret <- data.frame(Locus=loci_df,Gst=NA,Hs=NA,Ht=NA,P=NA)
    for( i in 1:length(loci_df) ){
      data <- loci[[loci_df[i]]] 
      r <- Gst( strata, data,  nperm, size.correct)
      ret[i,2:5] <- r
    }
    
    Gst.tot <- 1 - sum(ret$Hs, na.rm=T) / sum(ret$Ht,na.rm=T) 
    ret <- rbind( ret, c("Multilocus", Gst.tot,sum(ret$Hs,na.rm=T),sum(ret$Ht,na.rm=T),NA) )
    
    return( ret )
  }
  
  # do this for a single locus
  else {
    
    if( !is( loci, "locus") )
      stop(paste("This function requires objects of type 'locus' to function, you passed a '", class(loci), "' object",sep="" ))
    
    if( !is( strata, "factor") )
      strata <- factor( strata )
    
    k <- length( levels( strata ) )
    strata.lvls <- levels( strata )
    
    totfreq <- frequencies( loci )
    inds <- to_mv.locus( loci )
    p.vec <- colSums( inds )
    
    ht <- 1-sum( (p.vec / sum( p.vec ) )^2 )
    hs <- mean(unlist(lapply( strata.lvls, 
                              function(strat,inds,strata ) {
                                s <- colSums(as.matrix(inds[strata==strat,]))
                                f <- 1-sum((s/sum(s))^2)
                                return(f)
                              }, inds=inds, strata=strata)), na.rm=TRUE )
    
    if( ht == 0 ) {
      gst <- NA
      ht.estimated <- 0
      hs.estimated <- 0
    }
    
    else if( size.correct ) {
      n.harmonic <- 1/mean(1/table(strata))
      hs.estimated <- (2*n.harmonic)/(2*n.harmonic -1) * hs
      ht.estimated <- ht + hs.estimated/(2*k*n.harmonic)
      gst <- 1-hs.estimated/ht.estimated    
    }
    
    else {
      gst <- 1-hs/ht
      hs.estimated <- hs
      ht.estimated <- ht
    }
    
    
    
    
    
    if( nperm > 0 & ht > 0 ) {
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
        perms <- 1 - perms/ht.estimated      
      }
      else
        perms <- 1 - perms/ht
      
      perms <- perms[ !is.na(perms) ]
      
      P <- sum( perms >= gst ) / length(perms)
    }
    else 
      P <- NA
    
    
    ret <- data.frame( Gst=gst, Hs=hs.estimated, Ht=ht.estimated, P=P)
    
    return( ret ) 
  }  
}


