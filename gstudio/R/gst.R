#' Estimation Nei's Gst parameter
#' 
#' This function estimates Nei's Gst parameter and potentially
#'  returns the components of it as well as the probability.  The results are returned
#'  as a \code{data.frame}.
#' @param x  A vector of \code{\link{locus}} objects or a \code{data.frame} with \code{locus} objects.
#' @param stratum Either a vector of strata variables if \code{x} is a \code{locus} vector or 
#'  the name of the column representing strata in \code{x} if it is a \code{data.frame}.
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
#'  locus <- c(AA,AA,AA,AA,BB,BB,BB,AB,AB,AA)
#'  Population <- c(rep("Pop-A",5),rep("Pop-B",5))
#'  Gst( locus, Population, nperm=99 )
#'  locus2 <- c(AB,BB,AA,BB,BB,AB,AB,AA,AA,BB)
#'  df <- data.frame( Population, TPI=locus, PGM=locus2 )
#'  Gst( df, nperm=99)
Gst <- function( x, stratum="Population", nperm=0, size.correct=TRUE ) {
  
  # Do this function recursively if a data.frame is passed as loci
  if( is(x,"data.frame") ) {
    
    locus_names <- column_class(x,"locus")
    
    if( length(locus_names)==0 )
      stop("You must pass some loci to this function")
    
    if( !(stratum %in% names(x)) )
      stop("If you pass a data.frame to Gst(), you need to indicate a stratum varaible column.")
    
    strata <- x[[stratum]]
    K <- length(locus_names)
    ret <- data.frame(Locus=locus_names, Gst=numeric(K), Hs=numeric(K), Ht=numeric(K), P=numeric(K), stringsAsFactors=FALSE)

    for( i in 1:length(locus_names) ){
      data <- x[[locus_names[i]]] 
      r <- Gst( data, strata, nperm, size.correct)
      ret[i,2:5] <- r
      
    }
    
    Hs.tot <- sum(ret$Hs, na.rm=TRUE )
    Ht.tot <- sum(ret$Ht, na.rm=TRUE )
    Gst.tot <- 1 - Hs.tot / Ht.tot
    
    ret[K+1,1] <- "Multilocus"
    ret[K+1,2] <- Gst.tot
    ret[K+1,3] <- Hs.tot
    ret[K+1,4] <- Ht.tot

  }
  
  # do this for a single locus
  else {
    
    if( !is( x, "locus") )
      stop(paste("This function requires objects of type 'locus' to function, you passed a '", class(x), "' object",sep="" ))
    
    if( !is( stratum, "factor") )
      stratum <- factor( stratum )
    
    if( length(x) != length(stratum) )
      stop("You must pass loci and strata vectors of the same length to Gst().")
    
    k <- length( levels( stratum ) )
    strata.lvls <- levels( stratum )
    
    totfreq <- frequencies( x )
    inds <- to_mv.locus( x )
    p.vec <- colSums( inds )
    
    ht <- 1-sum( (p.vec / sum( p.vec ) )^2 )
    hs <- mean(unlist(lapply( strata.lvls, 
                              function(strat,inds,strata ) {
                                s <- colSums(as.matrix(inds[strata==strat,]))
                                f <- 1-sum((s/sum(s))^2)
                                return(f)
                              }, inds=inds, strata=stratum)), na.rm=TRUE )
    
    if( ht == 0 ) {
      gst <- NA
      ht.estimated <- 0
      hs.estimated <- 0
    }
    
    else if( size.correct ) {
      n.harmonic <- 1/mean(1/table(stratum))
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
                           strata=sample(stratum))
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
    

  }  
  return( ret ) 
}


