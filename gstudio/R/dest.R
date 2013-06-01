#' Estimation Joost's Dest parameter 
#' 
#' This function estimates the parameter (and potentially the confidence
#'  surrounding its value) for Joost's Dest.
#' @param x  A vector of \code{\link{locus}} objects or a \code{data.frame} with \code{locus} objects.
#' @param stratum Either a vector of strata variables if \code{x} is a \code{locus} vector or 
#'  the name of the column representing strata in \code{x} if it is a \code{data.frame}.
#' @param nperm The number of permutations to run for significance of the
#'  estimator.
#' @param size.correct A flag indicating that the estimate should be corrected for
#'  based upon sample sizes (default=TRUE).
#' @return A \code{data.frame} with Dest, Hs, Ht, and P (if asked for).  When multiple 
#'  loci are provided, the results also provide a multilocus estimate using the 
#'  harmonic mean.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#'  a1 <- sample( LETTERS[1:5], size=20, replace=TRUE)
#'  a2 <- sample( LETTERS[4:8], size=20, replace=TRUE)
#'  raw_alleles <- matrix( c(a1,a2), ncol=2, byrow=TRUE )
#'  locus <- locus( raw_alleles, type="column")
#'  Population <- c(rep("Pop-A",10),rep("Pop-B",10))
#'  Dest( locus, Population )
#'  a1 <- sample( LETTERS[1:5], size=20, replace=TRUE)
#'  a2 <- sample( LETTERS[4:8], size=20, replace=TRUE)
#'  raw_alleles <- matrix( c(a1,a2), ncol=2, byrow=TRUE )
#'  locus2 <- locus( raw_alleles, type="column")
#'  df <- data.frame( Population, TPI=locus, PGM=locus2 )
#'  Dest( df, nperm=99)
Dest <- function( x, stratum="Population", nperm=0, size.correct=FALSE ) {
  
  
  # Do this function recursively if a data.frame is passed as loci
  if( is(x,"data.frame") ) {
    
    locus_names <- column_class(x,"locus")
    
    if( length(locus_names)==0 )
      stop("You must pass some loci to this function")
    
    if( !(stratum %in% names(x)) )
      stop("If you pass a data.frame to Gst(), you need to indicate a stratum varaible column.")
    
    strata <- factor(x[[stratum]])
    K <- length(locus_names)
    ret <- data.frame(Locus=locus_names, Dest=numeric(K), Hs=numeric(K), Ht=numeric(K), P=numeric(K), stringsAsFactors=FALSE)
    
    for( i in 1:length(locus_names) ){
      data <- x[[locus_names[i]]] 
      r <- Dest( data, strata, nperm, size.correct)
      ret[i,2:5] <- r
      
    }
    
    k <- length(levels(strata))
    Hs.tot <- mean(ret$Hs, na.rm=TRUE )
    Ht.tot <- mean(ret$Ht, na.rm=TRUE )
    Dest.tot <- 1.0 / ( mean( 1/ret$Dest, na.rm=TRUE))

    
    ret[K+1,1] <- "Multilocus"
    ret[K+1,2] <- Dest.tot
    ret[K+1,3] <- Hs.tot
    ret[K+1,4] <- Ht.tot
    
  }
  
  # do this for a single locus
  else {
    
    if( !inherits(x,"locus") )
      stop("This function requires objects of type 'locus' to function.")
    
    if( !is(stratum,"factor") )
      stratum <- factor( stratum)
    
    if( length(stratum) != length(x))
      stop("If you are going to pass vector loci and strata, they should be the same size...")
    
    k <- length(levels(stratum))
    strata.lvls <- levels(stratum)
    
    totfreq <- frequencies( x )
    inds <- to_mv.locus( x )
    p.vec <- colSums(inds)
    ht <- 1-sum((p.vec/sum(p.vec))^2)
    hs <- mean(unlist(lapply( strata.lvls, 
                              function(strat,inds,strata ) {
                                s <- colSums(as.matrix(inds[strata==strat,]))
                                f <- 1-sum((s/sum(s))^2)
                              }, inds=inds, strata=stratum)),na.rm=TRUE)
    
    n.harmonic <- 1/mean(1/table(stratum))
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
                           function( strat, inds, strata ) {
                             s <- colSums( as.matrix( inds[ strata==strat, ]) )
                             f <- 1 - sum((s/sum(s))^2) 
                           }, 
                           inds=inds, 
                           strata=sample( stratum ) )
        perms[i] <- mean( unlist( hs.perm ) , na.rm=TRUE)
      }
      
      if( size.correct ) {
        perms <- (2*n.harmonic) / (2*n.harmonic-1) * perms
        D.perm <- ((ht.estimated-perms) / (1-perms))      
      }
      
      else
        D.perm <- (ht-perms) /(1-perms)
      
      D.perm <- D.perm / (k/(k-1))
      P <- sum( D.perm >= D ) / length( D.perm )
    }
    else
      P <- 0

    ret <- data.frame( Dest=D, Hs=hs.estimated, Ht=ht.estimated, P=P)    
  }
  
  return( ret )
}



